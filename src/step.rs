use crate::error::ParseError;
use std::borrow::Borrow;
use std::cmp::max;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use ParseError::{FailedOnValidation, ReachedEOF};
use Step::{Error, Fail, Success};

/// The intermediate result for atom parsing
#[derive(Debug, Clone)]
pub enum Step<'a, T> {
    /// Successfully resulted and current position is shifted forward
    Success(T, usize),
    /// Failed to parse amd current position remains the same
    Fail(usize),
    /// Unexpected situation pops up (like reached eof or some values can't pass a validation)
    Error(ParseError<'a>),
}

impl<'a, L, R> Step<'a, (L, R)> {
    /// the result if often is a pair of values.
    /// This method drops the right side and keeps only left and moves it farther
    /// It is used often when there is some token that should present in the particular place
    /// but does not carry extra sense like for a signature of a function we need to have brackets.
    ///
    /// # Example
    /// ```rust
    ///  use logos::Logos;
    ///  use crate::parsit::token;
    ///  use crate::parsit::parser::ParseIt;
    ///  use crate::parsit::parser::EmptyToken;
    ///  use crate::parsit::step::Step;
    ///
    ///  #[derive(Logos,PartialEq)]
    ///     pub enum T<'a> {
    ///         #[regex(r"[a-zA-Z-]+")]
    ///         Word(&'a str),
    ///
    ///         #[token("|")]
    ///         End,
    ///
    ///         #[error]
    ///         Error,
    ///     }
    ///
    ///  enum Word<'a>{ Word(&'a str), Bang(&'a str)}
    ///  let pit:ParseIt<T> = ParseIt::new("word|").unwrap();
    ///  let parse = |p:usize|{
    ///    token!(pit.token(p) => T::Word(v) => v)
    ///     .then_zip(|p| token!(pit.token(p) => T::End))
    ///     .take_left()
    ///  };
    ///
    ///
    /// ```
    pub fn take_left(self) -> Step<'a, L> {
        self.map(|(s, _)| s)
    }

    /// This method is a strict alias for the method `then` but for symmetric it presents
    pub fn take_right(self) -> Step<'a, R> {
        self.map(|(_, s)| s)
    }
}

impl<'a, L> Step<'a, (L, Vec<L>)> {
    /// Mergers the element from the left hand side to the right hand side
    /// This case often appears for the situations when there is a head of the same type
    /// and the tail that can be optional like
    ///```ebnf
    /// <rule> ::= <value> {"," <value>} // abc, bcd, cde
    /// ```
    /// Here the first value is always presented and the others can be absent.
    pub fn merge(self) -> Step<'a, Vec<L>> {
        self.map(|(h, mut rest)| {
            rest.insert(0, h);
            rest
        })
    }
}

impl<'a, L> Step<'a, (Option<L>, Vec<L>)> {
    /// Mergers the element from the left hand side to the right hand side if it exists
    /// This case often appears for the situations when there is a head of the same type
    /// and the tail that can be
    ///```ebnf
    /// <rule> ::= [value] {"," value }
    /// ```
    pub fn merge(self) -> Step<'a, Vec<L>> {
        self.map(|(h, mut rest)| match h {
            None => rest,
            Some(el) => {
                rest.insert(0, el);
                rest
            }
        })
    }
}

impl<'a, L: Eq + Hash, R> Step<'a, Vec<(L, R)>> {
    /// Mergers a vec of pairs into map
    /// This case often appears for the situations when there are some pair values and we know
    /// they are unique in names
    ///```ebnf
    /// <pair> ::= <item> ":" <item>
    /// <rule> ::= <pair> {"," <pair>}
    /// ```
    pub fn to_map(self) -> Step<'a, HashMap<L, R>> {
        self.map(|r| r.into_iter().collect::<HashMap<_, _>>())
    }
}

impl<'a, T> Step<'a, T> {
    /// Takes the next function and if the curren one and the next one are both success transforms the result value
    /// into a pair of them
    /// # Example
    /// ```rust
    ///  use logos::Logos;
    ///  use crate::parsit::token;
    ///  use crate::parsit::parser::ParseIt;
    ///  use crate::parsit::parser::EmptyToken;
    ///  use crate::parsit::step::Step;
    ///
    ///  #[derive(Logos,PartialEq)]
    ///     pub enum T<'a> {
    ///         #[regex(r"[a-zA-Z-]+")]
    ///         Word(&'a str),
    ///
    ///         #[token("|")]
    ///         Del,
    ///
    ///         #[error]
    ///         Error,
    ///     }
    ///
    ///  let pit:ParseIt<T> = ParseIt::new("word|another").unwrap();
    ///
    ///  let word = |p:usize| {token!(pit.token(p) => T::Word(v) => v)};
    ///  let del = |p:usize| {token!(pit.token(p) => T::Del)};
    ///
    ///  let parse = |p:usize|{
    ///     word(p)
    ///     .then_zip(del)
    ///     .take_left()
    ///     .then_zip(word)
    ///     .map(|(first,second)| format!("{},{}",first,second))
    ///  };
    ///
    ///
    /// ```
    pub fn then_zip<Res, Then>(self, then: Then) -> Step<'a, (T, Res)>
        where
            Then: FnOnce(usize) -> Step<'a, Res>,
    {
        self.then_combine(then, |a, b| (a, b))
    }
    /// Takes the next function and if the curren one and the next one are both success transforms the result value
    /// into a pair of them but if the second step is failed(absent)
    /// the second value will be replaced with a default value and the step will be succeeded.
    /// It is convenient when the second value is optional.
    /// # Example
    /// ```rust
    ///  use logos::Logos;
    ///  use crate::parsit::token;
    ///  use crate::parsit::parser::ParseIt;
    ///  use crate::parsit::parser::EmptyToken;
    ///  use crate::parsit::step::Step;
    ///
    ///  #[derive(Logos,PartialEq)]
    ///     pub enum T<'a> {
    ///         #[regex(r"[a-zA-Z-]+")]
    ///         Word(&'a str),
    ///
    ///         #[token("|")]
    ///         Del,
    ///
    ///         #[error]
    ///         Error,
    ///     }
    ///
    ///  enum Word<'a>{ Word(&'a str), Bang(&'a str)}
    ///  let pit:ParseIt<T> = ParseIt::new("word|").unwrap();
    ///
    ///  let word = |p:usize| {token!(pit.token(p) => T::Word(v) => v)};
    ///  let del = |p:usize| {token!(pit.token(p) => T::Del)};
    ///
    ///  let parse = |p:usize|{
    ///     word(p)
    ///     .then_zip(del)
    ///     .take_left()
    ///     .then_or_val_zip(word, &"default")
    ///     .map(|(first,second)| format!("{},{}",first,second))
    ///  };
    ///
    ///
    /// ```
    pub fn then_or_val_zip<Res, Then>(self, then: Then, default: Res) -> Step<'a, (T, Res)>
        where
            Then: FnOnce(usize) -> Step<'a, Res>,
    {
        self.then_or_val_combine(then, default, |a, b| (a, b))
    }

    /// Takes the next function and if the curren one and the next one are both success transforms the result value
    /// into a pair of them but if the second step is failed(absent)
    /// the second value will be replaced with a none and the step will be succeeded.
    /// It is convenient when the second value is optional.
    ///
    /// *Note : the second result should be an optional in any case*
    ///
    /// # Example
    /// ```rust
    ///  use logos::Logos;
    ///  use crate::parsit::token;
    ///  use crate::parsit::parser::ParseIt;
    ///  use crate::parsit::parser::EmptyToken;
    ///  use crate::parsit::step::Step;
    ///
    ///  #[derive(Logos,PartialEq)]
    ///     pub enum T<'a> {
    ///         #[regex(r"[a-zA-Z-]+")]
    ///         Word(&'a str),
    ///
    ///         #[token("|")]
    ///         Del,
    ///
    ///         #[error]
    ///         Error,
    ///     }
    ///
    ///  let pit:ParseIt<T> = ParseIt::new("word|").unwrap();
    ///
    ///  let word = |p:usize| {token!(pit.token(p) => T::Word(v) => v)};
    ///  let del = |p:usize| {token!(pit.token(p) => T::Del)};
    ///
    ///  let parse = |p:usize|{
    ///     word(p)
    ///     .then_zip(del)
    ///     .take_left()
    ///     .then_or_none_zip(|p| word(p).or_none()) // here we need to return option
    ///  };
    ///
    ///
    /// ```
    pub fn then_or_none_zip<Rhs, Then>(self, then: Then) -> Step<'a, (T, Option<Rhs>)>
        where
            Then: FnOnce(usize) -> Step<'a, Option<Rhs>>,
    {
        self.then_or_none_combine(then, |a, b| (a, b))
    }
    /// Takes the next function and if the curren one and the next one are both success transforms the result value
    /// into a pair of them but if the second step is failed(absent)
    /// the second value will be replaced with a default variant from the value
    /// and the step will be succeeded.
    ///
    /// *Note : the second result should implement default*
    ///
    /// # Example
    /// ```rust
    ///  use logos::Logos;
    ///  use crate::parsit::token;
    ///  use crate::parsit::parser::ParseIt;
    ///  use crate::parsit::parser::EmptyToken;
    ///  use crate::parsit::step::Step;
    ///
    ///  #[derive(Logos,PartialEq)]
    ///     pub enum T<'a> {
    ///         #[regex(r"[a-zA-Z-]+")]
    ///         Word(&'a str),
    ///
    ///         #[token("|")]
    ///         Del,
    ///
    ///         #[error]
    ///         Error,
    ///     }
    ///
    ///  enum Word<'a>{ Word(&'a str), Bang(&'a str)}
    ///  impl<'a> Default for Word<'a>{fn default() -> Self {
    ///         Word::Word("")
    ///     }}
    ///  let pit:ParseIt<T> = ParseIt::new("word|").unwrap();
    ///
    ///  let word = |p:usize| {token!(pit.token(p) => T::Word(v) => Word::Word(v))};
    ///  let del = |p:usize| {token!(pit.token(p) => T::Del)};
    ///
    ///  let parse = |p:usize|{
    ///     word(p)
    ///     .then_zip(del)
    ///     .take_left()
    ///     .then_or_default_zip(word) // here we return Word("word") and Word("")
    ///  };
    ///
    ///
    /// ```
    pub fn then_or_default_zip<Rhs: Default, Then>(self, then: Then) -> Step<'a, (T, Rhs)>
        where
            Then: FnOnce(usize) -> Step<'a, Rhs>,
    {
        self.then_or_val_zip(then, Rhs::default())
    }

    /// Combines this step with the next step according to the given function
    /// # Arguments
    /// * `then` - the next function to parse
    /// * `combine` - the mapper to combine to values
    ///
    /// This method is not used a lot directly but turns up as a foundation for others.
    /// For examples please see
    ///  - [Step.then](Step::then)
    ///  - [Step.then_zip](Step::then_zip)
    pub fn then_combine<Rhs, Res, Then, Combine>(
        self,
        then: Then,
        combine: Combine,
    ) -> Step<'a, Res>
        where
            Then: FnOnce(usize) -> Step<'a, Rhs>,
            Combine: FnOnce(T, Rhs) -> Res,
    {
        match self {
            Success(t, pos) => match then(pos) {
                Success(r, pos) => Success(combine(t, r), pos),
                Fail(pos) => Fail(pos),
                Error(e) => Error(e),
            },
            Fail(pos) => Fail(pos),
            Error(e) => Error(e),
        }
    }

    /// Applies the given function as many times as it will be succeeded and then combine the result
    ///
    /// Please see
    ///  - [Step::then_multi_zip](Step::then_multi_zip)
    ///  - [ParseIt::zero_or_more](ParseIt::zero_or_more)
    pub fn then_multi_combine<K, R, Then, Combine>(
        self,
        then: Then,
        combine: Combine,
    ) -> Step<'a, R>
        where
            Then: FnOnce(usize) -> Step<'a, K> + Copy,
            Combine: FnOnce(T, Vec<K>) -> R,
    {
        match self {
            Success(t, pos) => {
                let mut vals = vec![];
                let mut pos = pos;
                while let Success(r, next_pos) = then(pos) {
                    vals.push(r);
                    pos = next_pos
                }
                Success(combine(t, vals), pos)
            }
            Fail(pos) => Fail(pos),
            Error(e) => Error(e),
        }
    }
    /// Applies the given function as many times as it will be succeeded and then combine the result
    /// into a vec. It is used in the parser in methods `zero_or_more` or `one_or_more`
    ///
    pub fn then_multi_zip<R, Then>(self, then: Then) -> Step<'a, (T, Vec<R>)>
        where
            Then: FnOnce(usize) -> Step<'a, R> + Copy,
    {
        self.then_multi_combine(then, |f, v| (f, v))
    }

    /// Takes the next function and if the curren one and the next one are both success transforms the result value
    /// into a pair of them according to the given function combine
    ///
    /// For the details see [then_or_val_zip](Step::then_or_val_zip)
    pub fn then_or_val_combine<Rhs, Res, Then, Combine>(
        self,
        then: Then,
        default: Rhs,
        combine: Combine,
    ) -> Step<'a, Res>
        where
            Then: FnOnce(usize) -> Step<'a, Rhs>,
            Combine: FnOnce(T, Rhs) -> Res,
    {
        match self {
            Success(t, pos) => match then(pos) {
                Success(r, pos) => Success(combine(t, r), pos),
                Fail(pos) => Success(combine(t, default), pos),
                Error(ReachedEOF(pos)) => Success(combine(t, default), pos),
                Error(e) => Error(e),
            },
            Fail(pos) => Fail(pos),
            Error(e) => Error(e),
        }
    }
    /// Takes the next function and if the curren one and the next one are both success transforms the result value
    /// into a pair of them according to the given function combine and process the absent value as well
    ///
    /// For the details see [then_or_none_zip](Step::then_or_none_zip)
    pub fn then_or_none_combine<Rhs, Res, Then, Combine>(
        self,
        then: Then,
        combine: Combine,
    ) -> Step<'a, Res>
        where
            Then: FnOnce(usize) -> Step<'a, Option<Rhs>>,
            Combine: FnOnce(T, Option<Rhs>) -> Res,
    {
        self.then_or_val_combine(then, None, combine)
    }

    /// Takes and process the next step if the current one is succeed omitting the result of the current step.
    /// It is convenient when w need to know only a fact that the parser can parse current token
    /// without pulling out extra information from it
    ///
    ///# Example
    /// ```rust
    ///  use logos::Logos;
    ///  use crate::parsit::token;
    ///  use crate::parsit::parser::ParseIt;
    ///  use crate::parsit::parser::EmptyToken;
    ///  use crate::parsit::step::Step;
    ///
    ///  #[derive(Logos,PartialEq)]
    ///     pub enum T<'a> {
    ///         #[regex(r"[a-zA-Z-]+")]
    ///         Word(&'a str),
    ///
    ///         #[token("|")]
    ///         Del,
    ///
    ///         #[error]
    ///         Error,
    ///     }
    ///
    ///  enum Word<'a>{ Word(&'a str), Bang(&'a str)}
    ///  impl<'a> Default for Word<'a>{fn default() -> Self {
    ///         Word::Word("")
    ///     }}
    ///  let pit:ParseIt<T> = ParseIt::new("|word|").unwrap();
    ///
    ///  let word = |p:usize| {token!(pit.token(p) => T::Word(v) => Word::Word(v))};
    ///  let del = |p:usize| {token!(pit.token(p) => T::Del)};
    ///
    ///  let parse = |p:usize|{
    ///     del(p)
    ///         .then(word) // omitting the result of del
    ///         .then_zip(del)
    ///         .take_left()
    ///  };
    ///
    ///
    /// ```
    pub fn then<Rhs, Then>(self, then: Then) -> Step<'a, Rhs>
        where
            Then: FnOnce(usize) -> Step<'a, Rhs>,
    {
        self.then_combine(then, |_, k| k)
    }
    /// Takes and process the next step if the current one is succeed omitting the result of the current step.
    /// It is convenient when w need to know only a fact that the parser can parse current token
    /// without pulling out extra information from it
    ///
    /// If the next fn is not presented then return a default value
    ///
    ///# Example
    /// ```rust
    ///  use logos::Logos;
    ///  use crate::parsit::token;
    ///  use crate::parsit::parser::ParseIt;
    ///  use crate::parsit::parser::EmptyToken;
    ///  use crate::parsit::step::Step;
    ///
    ///  #[derive(Logos,PartialEq)]
    ///     pub enum T<'a> {
    ///         #[regex(r"[a-zA-Z-]+")]
    ///         Word(&'a str),
    ///
    ///         #[token("|")]
    ///         Del,
    ///
    ///         #[error]
    ///         Error,
    ///     }
    ///
    ///  enum Word<'a>{ Word(&'a str), Bang(&'a str), EmptyWord}
    ///  impl<'a> Default for Word<'a>{fn default() -> Self {
    ///         Word::Word("")
    ///     }}
    ///  let pit:ParseIt<T> = ParseIt::new("||").unwrap();
    ///
    ///  let word = |p:usize| {token!(pit.token(p) => T::Word(v) => Word::Word(v))};
    ///  let del = |p:usize| {token!(pit.token(p) => T::Del)};
    ///
    ///  let parse = |p:usize|{
    ///     del(p)
    ///         .then_or_val(word, Word::EmptyWord) // omitting the result of del and process empty var
    ///         .then_zip(del)
    ///         .take_left()
    ///  };
    ///
    ///
    /// ```
    pub fn then_or_val<Rhs, Then>(self, then: Then, default: Rhs) -> Step<'a, Rhs>
        where
            Then: FnOnce(usize) -> Step<'a, Rhs>,
    {
        match self {
            Success(_, pos) => then(pos).or_val(default),
            other => other.map(|_| default),
        }
    }
    /// Takes and process the next step if the current one is succeed omitting the result of the current step.
    /// It is convenient when w need to know only a fact that the parser can parse current token
    /// without pulling out extra information from it
    ///
    /// If the next fn is not presented then return a default value from trait
    ///
    ///# Example
    /// ```rust
    ///  use logos::Logos;
    ///  use crate::parsit::token;
    ///  use crate::parsit::parser::ParseIt;
    ///  use crate::parsit::parser::EmptyToken;
    ///  use crate::parsit::step::Step;
    ///
    ///  #[derive(Logos,PartialEq)]
    ///     pub enum T<'a> {
    ///         #[regex(r"[a-zA-Z-]+")]
    ///         Word(&'a str),
    ///
    ///         #[token("|")]
    ///         Del,
    ///
    ///         #[error]
    ///         Error,
    ///     }
    ///
    ///  enum Word<'a>{ Word(&'a str), Bang(&'a str), EmptyWord}
    ///  impl<'a> Default for Word<'a>{fn default() -> Self {
    ///         Word::EmptyWord
    ///     }}
    ///  let pit:ParseIt<T> = ParseIt::new("||").unwrap();
    ///
    ///  let word = |p:usize| {token!(pit.token(p) => T::Word(v) => Word::Word(v))};
    ///  let del = |p:usize| {token!(pit.token(p) => T::Del)};
    ///
    ///  let parse = |p:usize|{
    ///     del(p)
    ///         .then_or_default(word) // omitting the result of del and process empty var
    ///         .then_zip(del)
    ///         .take_left()
    ///  };
    ///
    ///
    /// ```
    pub fn then_or_default<Rhs: Default, Then>(self, then: Then) -> Step<'a, Rhs>
        where
            Then: FnOnce(usize) -> Step<'a, Rhs>,
    {
        match self {
            Success(_, pos) => then(pos).or_val(Rhs::default()),
            other => other.map(|_| Rhs::default()),
        }
    }
    /// Takes and process the next step if the current one is succeed omitting the result of the current step.
    /// It is convenient when w need to know only a fact that the parser can parse current token
    /// without pulling out extra information from it
    ///
    /// If the next fn is not presented then return a none
    ///
    ///# Example
    /// ```rust
    ///  use logos::Logos;
    ///  use crate::parsit::token;
    ///  use crate::parsit::parser::ParseIt;
    ///  use crate::parsit::parser::EmptyToken;
    ///  use crate::parsit::step::Step;
    ///
    ///  #[derive(Logos,PartialEq)]
    ///     pub enum T<'a> {
    ///         #[regex(r"[a-zA-Z-]+")]
    ///         Word(&'a str),
    ///
    ///         #[token("|")]
    ///         Del,
    ///
    ///         #[error]
    ///         Error,
    ///     }
    ///
    ///  enum Word<'a>{ Word(&'a str), Bang(&'a str), EmptyWord}
    ///  impl<'a> Default for Word<'a>{fn default() -> Self {
    ///         Word::Word("")
    ///     }}
    ///  let pit:ParseIt<T> = ParseIt::new("||").unwrap();
    ///
    ///  let word = |p:usize| {token!(pit.token(p) => T::Word(v) => Word::Word(v))};
    ///  let del = |p:usize| {token!(pit.token(p) => T::Del)};
    ///
    ///  let parse = |p:usize|{
    ///     del(p)
    ///         .then_or_none(|p| word(p).or_none()) // omitting the result of del and process empty var
    ///         .then_zip(del)
    ///         .take_left()
    ///  };
    ///
    ///
    /// ```
    pub fn then_or_none<Rhs, Then>(self, then: Then) -> Step<'a, Option<Rhs>>
        where
            Then: FnOnce(usize) -> Step<'a, Option<Rhs>>,
    {
        self.then_or_val(then, None)
    }
}

impl<'a, T: Debug> Step<'a, T> {
    /// Prints the information regarding the current step with a prefix

    /// The type To should implement debug
    ///
    /// # Examples
    /// ```rust
    /// use parsit::step::Step::Success;
    /// let step = Success(vec!["1"],0 );
    /// step.print();
    ///
    /// ```
    pub fn print(self) -> Step<'a, T> {
        self.print_with("")
    }
    /// Prints the information regarding the current step
    /// transforming the success according to the given function Show
    /// # Arguments
    /// * `show` - function transforming the value into some type that will be printed
    ///
    /// The type To should implement debug
    ///
    /// # Examples
    /// ```rust
    /// use parsit::step::Step::Success;
    /// let step = Success(vec!["1"],0 );
    /// step.print_as(|v| v.get(0).unwrap());
    ///
    /// ```
    pub fn print_as<Show, To>(self, show: Show) -> Step<'a, T>
        where
            Show: FnOnce(&T) -> &To,
            To: Debug,
    {
        self.print_with_as("", show)
    }

    /// Prints the information regarding the current step with a prefix
    /// # Example
    /// ```rust
    ///
    ///  use parsit::step::Step::Success;
    /// let step = Success(1,0);
    ///
    /// step
    /// .print_with("some_prefix")
    /// .map(|v| v + 1);
    /// // will be 'some_prefix success, pos: 0, res: 1'
    /// ```
    ///
    /// The success type should implement debug trait
    pub fn print_with(self, prefix: &'a str) -> Step<'a, T> {
        match self {
            Success(v, pos) => {
                println!(
                    "{} success, pos: {} , res: {:?}",
                    prefix, pos, v
                );
                Success(v, pos)
            }
            Fail(pos) => {
                println!("{} fail, pos: {}", prefix, pos);
                Fail(pos)
            }
            Error(e) => {
                println!("{} error {:?}", prefix, e);
                Error(e)
            }
        }
    }
    /// Prints the information regarding the current step with a prefix
    /// transforming the success according to the given function Show
    /// # Arguments
    /// * `prefix` - given prefix
    /// * `show` - function transforming the value into some type that will be printed
    ///
    /// The type To should implement debug
    ///
    /// # Examples
    /// ```rust
    /// use parsit::step::Step::Success;
    /// let step = Success(vec!["1"],0 );
    /// step.print_with_as("prefix", |v| v.get(0).unwrap());
    ///
    /// ```
    pub fn print_with_as<Show, To>(self, prefix: &'a str, show: Show) -> Step<'a, T>
        where
            Show: FnOnce(&T) -> &To,
            To: Debug,
    {
        match self {
            Success(v, pos) => {
                println!(
                    "debug | {} success, pos: {} , res: {:?}",
                    prefix,
                    pos,
                    show(v.borrow())
                );
                Success(v, pos)
            }
            Fail(pos) => {
                println!("debug | {} fail, pos: {}", prefix, pos);
                Fail(pos)
            }
            Error(e) => {
                println!("debug | {} error {:?}", prefix, e);
                Error(e)
            }
        }
    }
}

impl<'a, T> Step<'a, T> {
    /// Returns a value if the result is success otherwise returns none
    pub fn ok(self) -> Option<T> {
        match self {
            Success(t, _) => Some(t),
            _ => None
        }
    }
    /// Transforms a value if it is success
    ///
    /// # Examples
    /// ```rust
    /// use parsit::step::Step;
    /// let step_one = Step::Success(1,0);
    /// let step_two = step_one.map(|v|v + 1);
    ///
    /// ```
    ///
    pub fn map<Rhs, Map>(self, mapper: Map) -> Step<'a, Rhs>
        where
            Map: FnOnce(T) -> Rhs,
    {
        match self {
            Success(t, pos) => Success(mapper(t), pos),
            Fail(pos) => Fail(pos),
            Error(e) => Error(e),
        }
    }
    /// Combines the results from the current step(success) and with the given step success
    /// # Example
    /// ```rust
    ///
    /// use parsit::step::Step;
    ///
    /// let step = Step::Success(1,0);
    /// let next = step.combine(Step::Success(1,1),|a,b| a + b);
    ///
    /// ```
    /// # Notes
    ///  - If the result is successfull for both the latter position is taken
    ///  - If the result is failed the latest position is taken
    ///  - If the result is error the latest error is taken
    pub fn combine<Rhs, Res, Combine>(
        self,
        other: Step<'a, Rhs>,
        comb: Combine,
    ) -> Step<'a, Res>
        where
            Combine: FnOnce(T, Rhs) -> Res,
    {
        match (self, other) {
            (Success(t, l), Success(k, r)) => Success(comb(t, k), max(l, r)),
            (Fail(l), Fail(r)) => Fail(max(l, r)),
            (Error(e), _) | (_, Error(e)) => Error(e),
            (Fail(pos), _) | (_, Fail(pos)) => Fail(pos),
        }
    }
    /// Validates successful result and transforms into error if the validation is failed
    ///# Examples
    /// ```
    /// use parsit::error::ParseError;
    /// use parsit::step::Step;
    ///
    ///  let res = Step::Success("", 1);
    ///  let res = res.validate(|v| {
    ///        if v.len() > 0 { Ok(()) } else { Err("empty") }
    ///  });
    ///
    ///  if let Some(ParseError::FailedOnValidation(v,_)) = res.error(){
    ///     assert_eq!(v, "empty")
    ///  } else { assert!(false) };
    ///
    ///
    ///
    /// ```
    pub fn validate<Validation>(self, validate: Validation) -> Step<'a, T>
        where
            Validation: FnOnce(&T) -> Result<(), &'a str>,
    {
        match self {
            Success(r, pos) => match validate(r.borrow()) {
                Ok(_) => Success(r, pos),
                Err(mes) => Error(FailedOnValidation(mes, pos)),
            },
            other => other,
        }
    }
    /// Transforms error result into option if it is error or returns None otherwise
    /// # Example
    /// ```
    /// use parsit::error::ParseError;
    /// use parsit::step::Step;
    ///
    ///  let res = Step::Success("", 1);
    ///  let res = res.validate(|v| {
    ///        if v.len() > 0 { Ok(()) } else { Err("empty") }
    ///  });
    ///
    ///  if let Some(ParseError::FailedOnValidation(v,_)) = res.error(){
    ///     assert_eq!(v, "empty")
    ///  } else { assert!(false) };
    ///
    ///
    ///
    /// ```
    pub fn error(self) -> Option<ParseError<'a>> {
        match self {
            Error(e) => Some(e),
            _ => None
        }
    }
}

impl<'a, T> Step<'a, T> {
    /// Provides a default alternative for the given rule in case if the value does not present.
    ///
    /// This is an implementation for [or_val](Step::or_val) that provides a none as a default.
    /// It can be used to process an optional values when we have a specific default value to provide
    ///
    /// #Example
    ///
    /// ```ebnf
    /// <rule> ::= "value" ["!"]
    /// ```
    ///
    /// ```rust
    ///  use logos::Logos;
    ///  use crate::parsit::parser::ParseIt;
    ///  use crate::parsit::token;
    ///  use crate::parsit::step::Step;
    ///  use crate::parsit::parser::EmptyToken;
    ///  #[derive(Logos,PartialEq)]
    ///     pub enum T {
    ///         #[token("value")]
    ///         Value,
    ///         #[token("!")]
    ///         Bang,
    ///         #[error]
    ///         Error,
    ///     }
    ///  struct Value(bool);
    ///
    ///  let p:ParseIt<T> = ParseIt::new("value").unwrap();
    ///
    ///  let bang_opt = |pos| {
    ///         token!(p.token(pos) => T::Bang => true).or_val(false)
    ///     };
    ///
    ///  let parse =
    ///     token!(p.token(0) => T::Value)
    ///         .then_zip(bang_opt)
    ///         .take_right()
    ///         .map(Value);
    ///
    /// ```
    pub fn or_val(self, default: T) -> Step<'a, T> {
        match self {
            Fail(pos) => Success(default, pos),
            Error(ReachedEOF(pos)) => Success(default, pos),
            other => other,
        }
    }
    /// Provides an optional alternative for the given rule in case if the value does not present
    ///
    /// This is an implementation for [or_val](Step::or_val) that provides a none as a default.
    /// It can be used to process an optional values
    ///
    /// #Example
    /// ```rust
    ///  use logos::Logos;
    ///  use crate::parsit::parser::ParseIt;
    ///  use crate::parsit::token;
    ///  use crate::parsit::step::Step;
    ///  use crate::parsit::parser::EmptyToken;
    ///  #[derive(Logos,PartialEq)]
    ///     pub enum T {
    ///         #[token("value")]
    ///         Value,
    ///         #[token("!")]
    ///         Bang,
    ///         #[error]
    ///         Error,
    ///     }
    ///  struct Value(bool);
    ///
    ///  let p:ParseIt<T> = ParseIt::new("value").unwrap();
    ///
    ///  let bang_opt = |pos| {
    ///         token!(p.token(pos) => T::Bang).or_none()
    ///     };
    ///
    ///  let parse =
    ///     token!(p.token(0) => T::Value)
    ///         .then_zip(bang_opt)
    ///         .take_right()
    ///         .map(|opt| Value(opt.is_some()) );
    ///
    /// ```
    pub fn or_none(self) -> Step<'a, Option<T>> {
        self.map(|x| Some(x)).or_val(None)
    }
    /// Provides a function of backtracking in a horizon of one token.
    /// It works for non complex rules or for the rules that have a distinction in the beginning of parsing like
    /// ```bnf
    /// f: '[' items ']';
    /// s: '(' items ')';
    /// t: '{' items '}';
    ///
    /// rule: f | s | t ;
    ///
    /// ```
    /// # Example
    /// ```ignore
    ///
    ///  let first =  |p|{ token!(token => LBracket).then(items) };
    ///  let second = |p|{ token!(token => LParen).then(items) };
    ///  let third =  |p|{ token!(token => LSquare).then(items) };
    ///
    ///  first(pos)
    ///     .or(second)
    ///     .or(third)
    ///
    /// ```
    pub fn or<Alt>(self, next: Alt) -> Step<'a, T>
        where
            Alt: FnOnce(usize) -> Step<'a, T>,
    {
        match self {
            Fail(pos) => next(pos),
            Error(ReachedEOF(pos)) => next(pos),
            other => other,
        }
    }
    /// Declares the place where a backtracking can be performed.
    /// Details can be found in [Alt](Alt)
    pub fn or_from(self, pos: usize) -> Alt<'a, T> {
        Alt {
            init_pos: pos,
            current: self,
        }
    }
}

/// The structure is supposed to help with a backtracking from the given position.
/// # Example
/// ```ignore
///    // here we have alternatives for the text that ends with '.' or '?' or '!'
///             let sentence = |p| text(p)
///                 .then_zip(|p| token!(self.inner.token(p) => Token::Dot))
///                 .take_left()
///                 .map(Sentence::Sentence);
///
///             let exclamation = |p| text(p)
///                 .then_zip(|p| token!(self.inner.token(p) => Token::Bang))
///                 .take_left()
///                 .map(Sentence::Exclamation);
///
///             let question = |p| text(p)
///                 .then_zip(|p| token!(self.inner.token(p) => Token::Question))
///                 .take_left()
///                 .map(Sentence::Question);
///
///             sentence(pos)
///                 .or_from(pos)
///                 .or(exclamation)
///                 .or(question)
///                 .into()
///
/// ```
pub struct Alt<'a, T> {
    init_pos: usize,
    current: Step<'a, T>,
}

impl<'a, T> Alt<'a, T> {
    fn next<Next>(self, next: Next) -> Alt<'a, T>
        where
            Next: FnOnce(usize) -> Step<'a, T>,
    {
        Alt {
            init_pos: self.init_pos,
            current: next(self.init_pos),
        }
    }

    pub fn or<Next>(self, next: Next) -> Alt<'a, T>
        where
            Next: FnOnce(usize) -> Step<'a, T>,
    {
        match self.current {
            Fail(_) => self.next(next),
            Error(ReachedEOF(_)) => self.next(next),
            other => Alt {
                init_pos: self.init_pos,
                current: other,
            },
        }
    }
}



impl<'a, T> From<Alt<'a, T>> for Step<'a, T> {
    fn from(alt: Alt<'a, T>) -> Self {
       alt.current
    }
}

impl<'a, T> From<Step <'a,T>> for Result<T, ParseError<'a>> {
    fn from(step: Step<'a, T>) -> Self {
        match step {
            Success(t, _) => Ok(t),
            Fail(_) => Err(ParseError::FinishedOnFail),
            Error(e) => Err(e),
        }
    }
}
