pub mod lexer_test {
    use std::fmt::Debug;
    use logos::Logos;
    use crate::error::ParseError;
    use crate::lexer::LexIt;


    /// ensures the given output is expected
    ///
    /// # Example
    /// ```rust
    /// use logos::Logos;
    /// use crate::parsit::test::lexer_test::expect;
    ///
    /// #[derive(Logos, Debug, PartialEq)]
    /// pub enum T<'a> {
    ///     #[regex(r"[a-zA-Z-]+")]
    ///     Word(&'a str),
    ///
    ///     #[token(",")]
    ///     Comma,
    ///     #[token(".")]
    ///     Dot,
    ///
    ///     #[token("!")]
    ///     Bang,
    ///     #[token("?")]
    ///     Question,
    ///
    ///     #[regex(r"[ \t\r\n]+", logos::skip)]
    ///     Whitespace,
    ///     #[error]
    ///     Error,
    /// }
    ///     expect::<T>(
    ///         "abc, bcs!",
    ///         vec![T::Word("abc"), T::Comma, T::Word("bcs"), T::Bang]
    ///     )
    /// ```
    pub fn expect<'a, T>(src: &'a str, tokens: Vec<T>)
        where
            T: Logos<'a, Source=str> + Debug + PartialEq,
            T::Extras: Default,
    {
        match LexIt::<T>::new(src) {
            Ok(lexer) => assert_eq!(lexer.tokens, tokens),
            Err(error) => panic!("the test failed due to error: {:?}", error),
        }
    }

    /// ensures the lexical analysis is done correctly
    ///
    /// # Example
    /// ```rust
    /// use logos::Logos;
    /// use parsit::test::lexer_test::expect_succeed;
    /// use crate::parsit::test::lexer_test::expect;
    ///
    /// #[derive(Logos, Debug, PartialEq)]
    /// pub enum T<'a> {
    ///     #[regex(r"[a-zA-Z-]+")]
    ///     Word(&'a str),
    ///
    ///     #[token(",")]
    ///     Comma,
    ///     #[token(".")]
    ///     Dot,
    ///
    ///     #[token("!")]
    ///     Bang,
    ///     #[token("?")]
    ///     Question,
    ///
    ///     #[regex(r"[ \t\r\n]+", logos::skip)]
    ///     Whitespace,
    ///     #[error]
    ///     Error,
    /// }
    ///     expect_succeed::<T>("abc, bcs!");
    /// ```
    pub fn expect_succeed<'a, T>(src: &'a str)
        where
            T: Logos<'a, Source=str> + PartialEq,
            T::Extras: Default,
    {
        match LexIt::<T>::new(src) {
            Ok(_) => (),
            Err(error) => panic!("the test failed due to an error: {:?}", error),
        }
    }

    /// ensures the lexical analysis is failed
    ///
    /// # Example
    /// ```rust
    /// use logos::Logos;
    /// use parsit::test::lexer_test::{expect_failed, expect_succeed};
    /// use crate::parsit::test::lexer_test::expect;
    ///
    /// #[derive(Logos, Debug, PartialEq)]
    /// pub enum T<'a> {
    ///     #[regex(r"[a-zA-Z-]+")]
    ///     Word(&'a str),
    ///
    ///     #[token(",")]
    ///     Comma,
    ///     #[token(".")]
    ///     Dot,
    ///
    ///     #[token("!")]
    ///     Bang,
    ///     #[token("?")]
    ///     Question,
    ///
    ///     #[regex(r"[ \t\r\n]+", logos::skip)]
    ///     Whitespace,
    ///     #[error]
    ///     Error,
    /// }
    ///     expect_failed::<T>("abc, bcs >> !");
    /// ```
    pub fn expect_failed<'a, T>(src: &'a str)
        where
            T: Logos<'a, Source=str> + PartialEq,
            T::Extras: Default,
    {
        match LexIt::<T>::new(src) {
            Ok(_) => panic!("the test failed due to expectation to get an error but tokens have been parsed"),
            Err(_) => (),
        }
    }

    /// ensures the lexical analysis is failed with a proper error
    ///
    /// # Example
    /// ```rust
    /// use logos::Logos;
    /// use parsit::test::lexer_test::{expect_failed_with, expect_succeed};
    /// use crate::parsit::test::lexer_test::expect;
    ///
    /// #[derive(Logos, Debug, PartialEq)]
    /// pub enum T<'a> {
    ///     #[regex(r"[a-zA-Z-]+")]
    ///     Word(&'a str),
    ///
    ///     #[token(",")]
    ///     Comma,
    ///     #[token(".")]
    ///     Dot,
    ///
    ///     #[token("!")]
    ///     Bang,
    ///     #[token("?")]
    ///     Question,
    ///
    ///     #[regex(r"[ \t\r\n]+", logos::skip)]
    ///     Whitespace,
    ///     #[error]
    ///     Error,
    /// }
    ///     expect_failed_with::<T,_>("abc, bcs > !", |e| e.is_bad_token_on(">") );
    /// ```
    pub fn expect_failed_with<'a, T, Error>(src: &'a str, error: Error)
        where
            T: Logos<'a, Source=str> + PartialEq,
            T::Extras: Default,
            Error: FnOnce(ParseError<'a>) -> ()
    {
        match LexIt::<T>::new(src) {
            Ok(lexer) => panic!("the test failed due to expectation to get an error but tokens have been parsed"),
            Err(e) => error(e),
        }
    }

    impl<'a> ParseError<'a> {
        pub fn is_bad_token_on(&self, src: &'a str) -> () {
            match &self {
                ParseError::BadToken(s, _) => assert_eq!(*s, src),
                _ => ()
            }
        }
    }
}
pub mod parser_test {
    use std::fmt::Debug;
    use logos::Logos;
    use crate::parser::ParseIt;
    use crate::step::Step;

    /// Creates an internal parser or panicking otherwise
    pub fn parsit<'a, T>(src: &'a str) -> ParseIt<T>
        where
            T: Logos<'a, Source=str> + Debug + PartialEq,
            T::Extras: Default,
    {
        match ParseIt::new(src) {
            Ok(p) => p,
            Err(e) => panic!("Some error popped up in lexing, namely {:?}", e),
        }
    }

    /// Asserts a step value with a given one
    /// # Example
    /// ```rust
    /// use logos::Logos;
    /// use parsit::test::parser_test::expect;
    /// use crate::parsit::test::parser_test::*;
    /// use crate::parsit::token;
    /// use crate::parsit::parser::ParseIt;
    /// use crate::parsit::step::Step;
    ///
    /// #[derive(Logos, Debug, PartialEq)]
    /// pub enum T<'a> {
    ///     #[regex(r"[a-zA-Z-]+")]
    ///     Word(&'a str),
    ///
    ///     #[token(",")]
    ///     Comma,
    ///     #[token(".")]
    ///     Dot,
    ///
    ///     #[token("!")]
    ///     Bang,
    ///     #[token("?")]
    ///     Question,
    ///
    ///     #[regex(r"[ \t\r\n]+", logos::skip)]
    ///     Whitespace,
    ///     #[error]
    ///     Error,
    /// }
    /// let p = parsit("abc!");
    /// let bang = |pos:usize| token!(p.token(pos) => T::Bang => "!");
    /// let word = |pos:usize| token!(p.token(pos) => T::Word(v) => *v);
    /// let step =
    ///     word(0)
    ///         .then_or_val_zip(bang, "")
    ///         .map(|(a,b)| format!("{}{}",a,b));
    ///
    /// expect(step,"abc!".to_string());
    /// ```
    pub fn expect<T>(res: Step<T>, expect: T)
        where
            T: PartialEq + Debug,
    {
        match res {
            Step::Success(v, _) => assert_eq!(v, expect),
            Step::Fail(pos) => panic!("A step failed on a position {}", pos),
            Step::Error(e) => panic!("A step finished with an error {:?}", e),
        }
    }
    /// Asserts a step value with success state and the position is expected as well
    /// # Example
    /// ```rust
    /// use logos::Logos;
    /// use crate::parsit::test::parser_test::expect_pos;
    /// use crate::parsit::test::parser_test::parsit;
    /// use crate::parsit::token;
    /// use crate::parsit::parser::ParseIt;
    /// use crate::parsit::step::Step;
    ///
    /// #[derive(Logos, Debug, PartialEq)]
    /// pub enum T<'a> {
    ///     #[regex(r"[a-zA-Z-]+")]
    ///     Word(&'a str),
    ///
    ///     #[token(",")]
    ///     Comma,
    ///     #[token(".")]
    ///     Dot,
    ///
    ///     #[token("!")]
    ///     Bang,
    ///     #[token("?")]
    ///     Question,
    ///
    ///     #[regex(r"[ \t\r\n]+", logos::skip)]
    ///     Whitespace,
    ///     #[error]
    ///     Error,
    /// }
    /// let p = parsit("abc!");
    /// let bang = |pos:usize| token!(p.token(pos) => T::Bang => "!");
    /// let word = |pos:usize| token!(p.token(pos) => T::Word(v) => v);
    /// let step =word(0).then_or_val_zip(bang, "");
    ///
    /// expect_pos(step,2); // the next position to parse
    /// ```
    pub fn expect_pos<T>(res: Step<T>, expect: usize) {
        match res {
            Step::Success(_, pos) => assert_eq!(pos, expect),
            Step::Fail(pos) => panic!("A step failed on a position {}", pos),
            Step::Error(e) => panic!("A step finished with an error {:?}", e),
        }
    }
    /// Asserts a step value with a fail.
    /// Fail is related only on the fail in the given rule in a step.
    /// # Example
    /// ```rust
    /// use logos::Logos;
    /// use crate::parsit::test::parser_test::fail;
    /// use crate::parsit::test::parser_test::parsit;
    /// use crate::parsit::token;
    /// use crate::parsit::parser::ParseIt;
    /// use crate::parsit::step::Step;
    ///
    /// #[derive(Logos, Debug, PartialEq)]
    /// pub enum T<'a> {
    ///     #[regex(r"[a-zA-Z-]+")]
    ///     Word(&'a str),
    ///
    ///     #[token(",")]
    ///     Comma,
    ///     #[token(".")]
    ///     Dot,
    ///
    ///     #[token("!")]
    ///     Bang,
    ///     #[token("?")]
    ///     Question,
    ///     #[token(">")]
    ///     Gt,
    ///
    ///     #[regex(r"[ \t\r\n]+", logos::skip)]
    ///     Whitespace,
    ///     #[error]
    ///     Error,
    /// }
    /// let p = parsit("abc>!");
    /// let bang = |pos:usize| token!(p.token(pos) => T::Bang => "!");
    /// let word = |pos:usize| token!(p.token(pos) => T::Word(v) => v);
    /// let step =word(0).then_zip(bang);
    ///
    /// fail(step);
    /// ```
    pub fn fail<T>(res: Step<T>) {
        match res {
            Step::Success(v, pos) => {
                panic!("A step should failed but successes on a position {}", pos)
            }
            Step::Fail(_) => (),
            Step::Error(e) => panic!("A step should failed but errored with an error {:?}", e),
        }
    }
    /// Asserts a step value with a fail on an expected position
    /// Fail is related only on the fail in the given rule in a step.
    /// # Example
    /// ```rust
    /// use logos::Logos;
    /// use crate::parsit::test::parser_test::fail_on;
    /// use crate::parsit::test::parser_test::parsit;
    /// use crate::parsit::token;
    /// use crate::parsit::parser::ParseIt;
    /// use crate::parsit::step::Step;
    ///
    /// #[derive(Logos, Debug, PartialEq)]
    /// pub enum T<'a> {
    ///     #[regex(r"[a-zA-Z-]+")]
    ///     Word(&'a str),
    ///
    ///     #[token(",")]
    ///     Comma,
    ///     #[token(".")]
    ///     Dot,
    ///
    ///     #[token("!")]
    ///     Bang,
    ///     #[token("?")]
    ///     Question,
    ///     #[token(">")]
    ///     Gt,
    ///
    ///     #[regex(r"[ \t\r\n]+", logos::skip)]
    ///     Whitespace,
    ///     #[error]
    ///     Error,
    /// }
    /// let p = parsit("abc>!");
    /// let bang = |pos:usize| token!(p.token(pos) => T::Bang => "!");
    /// let word = |pos:usize| token!(p.token(pos) => T::Word(v) => v);
    /// let step =word(0).then_zip(bang);
    ///
    /// fail_on(step,1);
    /// ```
    pub fn fail_on<T>(res: Step<T>, expect: usize) {
        match res {
            Step::Success(v, pos) => {
                panic!("A step should failed but successes on a position {}", pos)
            }
            Step::Fail(pos) => assert_eq!(pos, expect),
            Step::Error(e) => panic!("A step should failed but errored with an error {:?}", e),
        }
    }
}
