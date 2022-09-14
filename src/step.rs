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
    ///```antlr
    /// contract: value (COMMA value)* // abc, bcd, cde
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
    ///```antlr
    /// contract: value? (COMMA value)+
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
    ///```antlr
    /// pair: item:item;
    /// contract: pair (COMMA pair)*
    /// ```
    pub fn to_map(self) -> Step<'a, HashMap<L, R>> {
        self.map(|r| r.into_iter().collect::<HashMap<_, _>>())
    }
}

impl<'a, T> Step<'a, T> {
    pub fn then_zip<Res, Then>(self, then: Then) -> Step<'a, (T, Res)>
        where
            Then: FnOnce(usize) -> Step<'a, Res>,
    {
        self.then_combine(then, |a, b| (a, b))
    }
    pub fn then_or_val_zip<Res, Then>(self, then: Then, default: Res) -> Step<'a, (T, Res)>
        where
            Then: FnOnce(usize) -> Step<'a, Res>,
    {
        self.then_or_val_combine(then, default, |a, b| (a, b))
    }

    pub fn then_or_none_zip<Rhs, Then>(self, then: Then) -> Step<'a, (T, Option<Rhs>)>
        where
            Then: FnOnce(usize) -> Step<'a, Option<Rhs>>,
    {
        self.then_or_none_combine(then, |a, b| (a, b))
    }
    pub fn then_or_default_zip<Rhs: Default, Then>(self, then: Then) -> Step<'a, (T, Rhs)>
        where
            Then: FnOnce(usize) -> Step<'a, Rhs>,
    {
        self.then_or_val_zip(then, Rhs::default())
    }

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
                loop {
                    match then(pos) {
                        Success(r, next_pos) => {
                            vals.push(r);
                            pos = next_pos
                        }
                        _ => break,
                    }
                }
                Success(combine(t, vals), pos)
            }
            Fail(pos) => Fail(pos),
            Error(e) => Error(e),
        }
    }
    pub fn then_multi_zip<R, Then>(self, then: Then) -> Step<'a, (T, Vec<R>)>
        where
            Then: FnOnce(usize) -> Step<'a, R> + Copy,
    {
        self.then_multi_combine(then, |f, v| (f, v))
    }

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

    pub fn then<Rhs, Then>(self, then: Then) -> Step<'a, Rhs>
        where
            Then: FnOnce(usize) -> Step<'a, Rhs>,
    {
        self.then_combine(then, |_, k| k)
    }

    pub fn then_or_val<Rhs, Then>(self, then: Then, default: Rhs) -> Step<'a, Rhs>
        where
            Then: FnOnce(usize) -> Step<'a, Rhs>,
    {
        match self {
            Success(_, pos) => then(pos).or_val(default),
            other => other.map(|_| default),
        }
    }
    pub fn then_or_default<Rhs: Default, Then>(self, then: Then) -> Step<'a, Rhs>
        where
            Then: FnOnce(usize) -> Step<'a, Rhs>,
    {
        match self {
            Success(_, pos) => then(pos).or_val(Rhs::default()),
            other => other.map(|_| Rhs::default()),
        }
    }
    pub fn then_or_none<Rhs, Then>(self, then: Then) -> Step<'a, Option<Rhs>>
        where
            Then: FnOnce(usize) -> Step<'a, Option<Rhs>>,
    {
        self.then_or_val(then, None)
    }
}

impl<'a, Rhs: Debug, Lhs: Debug> Step<'a, (Lhs, Rhs)> {
    pub fn debug1_show_last(self, prefix: &'a str) -> Step<'a, (Lhs, Rhs)> {
        self.debug1_show(prefix, |(_, x)| x)
    }
    pub fn debug_show_last(self) -> Step<'a, (Lhs, Rhs)> {
        self.debug_show(|(_, x)| x)
    }
}

impl<'a, T: Debug> Step<'a, T> {
    pub fn debug(self) -> Step<'a, T> {
        self.debug1("")
    }
    pub fn debug_show<Show, To>(self, show: Show) -> Step<'a, T>
        where
            Show: FnOnce(&T) -> &To,
            To: Debug,
    {
        self.debug1_show("", show)
    }
    pub fn debug1(self, prefix: &'a str) -> Step<'a, T> {
        match self {
            Success(v, pos) => {
                println!(
                    "debug | {} success, pos: {} , res: {:?}",
                    prefix, pos, v
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
    pub fn debug1_show<Show, To>(self, prefix: &'a str, show: Show) -> Step<'a, T>
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
    pub fn ok(self) -> Option<T> {
        match self {
            Success(t, _) => Some(t),
            _ => None
        }
    }
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

    pub fn map_error<R, E>(self, mapper: E) -> Option<R> where E: FnOnce(ParseError<'a>) -> R {
        match self {
            Error(e) => Some(mapper(e)),
            other => None
        }
    }
}

impl<'a, T> Step<'a, T> {
    pub fn or_val(self, default: T) -> Step<'a, T> {
        match self {
            Fail(pos) => Success(default, pos),
            Error(ReachedEOF(pos)) => Success(default, pos),
            other => other,
        }
    }
    pub fn or_none(self) -> Step<'a, Option<T>> {
        self.map(|x| Some(x)).or_val(None)
    }
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
    /// declares the place where a backtracking can be performed.
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
///  // here we have alternatives for the text that ends with '.' or '?' or '!'
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

impl<'a, T> Into<Step<'a, T>> for Alt<'a, T> {
    fn into(self) -> Step<'a, T> {
        self.current
    }
}

impl<'a, T> Into<Result<T, ParseError<'a>>> for Step<'a, T> {
    fn into(self) -> Result<T, ParseError<'a>> {
        match self {
            Success(t, _) => Ok(t),
            Fail(_) => Err(ParseError::FinishedOnFail),
            Error(e) => Err(e),
        }
    }
}
