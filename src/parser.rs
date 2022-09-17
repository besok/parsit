use logos::Logos;
use crate::lexer::LexIt;
use crate::error::ParseError;
use crate::error::ParseError::{ReachedEOF, UnreachedEOF};
use crate::step::Step;
use crate::step::Step::{Error, Fail, Success};


/// The base structure of the parser combinator that providers a set of methods to construct a gramma
///
/// To construct the parser it takes a token set from `Logos` typically as an Enum
///
/// ```
///  use logos::Logos;
///  use crate::parsit::parser::ParseIt;
///  #[derive(Logos,PartialEq)]
///     pub enum TFQ {
///         #[token("true")]
///         True,
///         #[token("false")]
///         False,
///
///         #[token("?")]
///         Question,
///
///         #[error]
///         Error,
///     }
///  let p:ParseIt<TFQ> = ParseIt::new("true?").unwrap();
/// ```
/// Note: The parser works only with string input
///
pub struct ParseIt<'a, T> where T: Logos<'a, Source=str>, {
    lexer: LexIt<'a, T>,
}

impl<'a, Token> ParseIt<'a, Token>
    where Token: Logos<'a, Source=str> + PartialEq,
{
    /// Creates a parser with aset of tokens from the source
    /// Raises an error if lexer raises an error
    pub fn new(src: &'a str) -> Result<Self, ParseError<'a>>
        where Token::Extras: Default
    {
        Ok(ParseIt {
            lexer: LexIt::new(src)?,
        })
    }

    /// obtain a token from the parsing output according to the position number
    ///
    /// # Arguments
    /// * `pos` position number
    ///
    /// # Examples
    /// typically used in the token! macros:
    /// ```ignore
    ///  let p:ParseIt<_> = ParseIt::new(...)?;
    ///  token!(p.token(0) => ...)
    /// ```
    pub fn token(&self, pos: usize) -> Result<(&Token, usize), ParseError<'a>> {
        self.lexer.token(pos)
    }

    /// executes some rule one or more times shifting the cursor farther.
    /// The operator expects at least one occurance of the given function
    /// # Arguments
    /// * `pos` - starting postion to parse
    /// * `then` - parsing function
    ///
    /// # Examples
    /// ```
    ///  use logos::Logos;
    ///  use crate::parsit::parser::ParseIt;
    ///  use crate::parsit::token;
    ///  use crate::parsit::step::Step;
    ///  #[derive(Logos,PartialEq)]
    ///     pub enum TFQ {
    ///         #[token("true")]
    ///         True,
    ///         #[token("false")]
    ///         False,
    ///
    ///         #[token("?")]
    ///         Question,
    ///
    ///         #[error]
    ///         Error,
    ///     }
    ///  let parser:ParseIt<TFQ> = ParseIt::new("true?false").unwrap();
    ///  let parser_fn = |p|{ token!( parser.token(p) =>
    ///                   TFQ::True => Some(true),
    ///                   TFQ::False => Some(false),
    ///                   TFQ::Question => None
    ///             )};
    ///
    ///   if let Some(res) = parser.one_or_more(0, parser_fn).ok() {
    ///     assert_eq!(res, vec![Some(true), None, Some(false)]);
    ///   } else { assert!(false) };
    /// ```
    pub fn one_or_more<T, Then>(&self, pos: usize, then: Then) -> Step<'a, Vec<T>>
        where
            Then: FnOnce(usize) -> Step<'a, T> + Copy,
    {
        match self.zero_or_more(pos, then) {
            Success(vals, _) if vals.is_empty() => Fail(pos),
            other => other,
        }
    }
    /// executes some rule one or more times shifting the cursor farther.
    /// The operator expects zero or more occurances of the given function
    /// # Arguments
    /// * `pos` - starting postion to parse
    /// * `then` - parsing function
    ///
    /// # Examples
    /// ```
    ///  use logos::Logos;
    ///  use crate::parsit::parser::ParseIt;
    ///  use crate::parsit::token;
    ///  use crate::parsit::step::Step;
    ///  #[derive(Logos,PartialEq)]
    ///     pub enum TFQ {
    ///         #[token("true")]
    ///         True,
    ///         #[token("false")]
    ///         False,
    ///
    ///         #[token("?")]
    ///         Question,
    ///
    ///         #[error]
    ///         Error,
    ///     }
    ///  let parser:ParseIt<TFQ> = ParseIt::new("").unwrap();
    ///  let parser_fn = |p|{ token!( parser.token(p) =>
    ///                   TFQ::True => Some(true),
    ///                   TFQ::False => Some(false),
    ///                   TFQ::Question => None
    ///             )};
    ///
    ///   if let Some(res) = parser.zero_or_more(0, parser_fn).ok() {
    ///     assert_eq!(res, vec![]);
    ///   } else { assert!(false) };
    /// ```
    pub fn zero_or_more<T, Then>(&self, pos: usize, then: Then) -> Step<'a, Vec<T>>
        where
            Then: FnOnce(usize) -> Step<'a, T> + Copy,
    {
        match then(pos).then_multi_zip(then).merge() {
            Fail(_) => Success(vec![], pos),
            Error(ReachedEOF(_)) => Success(vec![], pos),
            success => success,
        }
    }
    /// Validates if the parsing process reaches the end of the input.
    /// If so transforms the result to the error UnreachedEOF
    ///
    /// It can be used in the end of the parsing
    pub fn validate_eof<T>(&self, res: Step<'a, T>) -> Step<'a, T> {
        match res {
            Success(_, pos) if self.lexer.len() != pos => Error(UnreachedEOF(pos)),
            other => other,
        }
    }
}

/// The token is used as a stub for the parsing operations when we need just a notion
/// that the token is parsed correctly but we don't need to process any values.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct EmptyToken {}

/// Helps to parse a token on the lexer level
///  - Typically, it takes a token from the parser(defined in the structure of Logos)
///  - The structure should implement `PartialEq`
///
/// # Examples
/// - create a pattern matching for the given tokens
/// ```
///     use logos::Logos;
///     use crate::parsit::parser::ParseIt;
///     use crate::parsit::token;
///     use crate::parsit::step::Step;
///     use crate::parsit::parser::EmptyToken;
///     #[derive(Logos,PartialEq)]
///     pub enum TFQ {
///         #[token("true")]
///         True,
///         #[token("false")]
///         False,
///
///         #[token("?")]
///         Question,
///
///
///         #[error]
///         Error,
///     }
///
///     let p:ParseIt<TFQ> = ParseIt::new("true?").unwrap();
///     // create a pattern matching for the given tokens
///      token!(
///         p.token(0) =>
///             TFQ::True => true,
///             TFQ::False => false
///      );
///     // create a matching for only one token without a result
///     // it is used oftenly with then().
///     // The EmptyToken will be return
///      token!(p.token(1) => TFQ::Question);
///
/// ```
///
#[macro_export]
macro_rules! token {
  ($obj:expr => $($matcher:pat $(if $pred:expr)* => $result:expr),*) => {
      match $obj {
            Ok((t,p)) => match t {
                $($matcher $(if $pred)* => Step::Success($result, p + 1)),*,
                _ => Step::Fail(p)
            }
            Err(e) => Step::Error(e)
        }

   };
  ($obj:expr => $($matcher:pat $(if $pred:expr)*),*) => {
      match $obj {
            Ok((t,p)) => match t {
                $($matcher $(if $pred)* => Step::Success(EmptyToken{}, p + 1)),*,
                _ => Step::Fail(p)
            }
            Err(e) => Step::Error(e)
        }

   }

}