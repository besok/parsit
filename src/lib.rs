//!The crate provides a basic mechanism to parse grammars using Logo as a lexer
//! and a simple recursive descendant LL(1) parser with a backtracking function.
//!
//! # Example
//!```
//!     use crate::parsit::token;
//!     use crate::parsit::step::Step;
//!     use crate::parsit::parser::EmptyToken;
//!     use crate::parsit::parser::ParseIt;
//!     use crate::parsit::error::ParseError;
//!     use logos::Logos;
//!
//!
//!     #[derive(Logos, Debug, Copy, Clone, PartialEq)]
//!     pub enum Token<'a> {
//!         #[regex(r"[a-zA-Z-]+")]
//!         Word(&'a str),
//!
//!         #[token(",")]
//!         Comma,
//!         #[token(".")]
//!         Dot,
//!
//!         #[token("!")]
//!         Bang,
//!         #[token("?")]
//!         Question,
//!
//!         #[regex(r"[ \t\r\n]+", logos::skip)]
//!         Whitespace,
//!     }
//!
//!     #[derive(Debug, Copy, Clone, PartialEq)]
//!     enum Item<'a> {
//!         Word(&'a str),
//!         Comma,
//!     }
//!     #[derive(Debug, Clone, PartialEq)]
//!     enum Sentence<'a> {
//!         Sentence(Vec<Item<'a>>),
//!         Question(Vec<Item<'a>>),
//!         Exclamation(Vec<Item<'a>>),
//!     }
//!
//!     struct Parser<'a> {
//!         inner: ParseIt<'a, Token<'a>>,
//!     }
//!
//!     impl<'a> Parser<'a> {
//!         fn new(text: &'a str) -> Parser<'a> {
//!             let delegate: ParseIt<Token> = ParseIt::new(text).unwrap();
//!             Parser { inner: delegate }
//!         }
//!
//!         fn sentence(&self, pos: usize) -> Step<'a,Sentence<'a>> {
//!             let items = |p| self.inner.one_or_more(p, |p| self.word(p));
//!
//!             let sentence = |p| items(p)
//!                 .then_zip(|p| token!(self.inner.token(p) => Token::Dot))
//!                 .take_left()
//!                 .map(Sentence::Sentence);
//!
//!             let exclamation = |p| items(p)
//!                 .then_zip(|p| token!(self.inner.token(p) => Token::Bang))
//!                 .take_left()
//!                 .map(Sentence::Exclamation);
//!             let question = |p| items(p)
//!                 .then_zip(|p| token!(self.inner.token(p) => Token::Question))
//!                 .take_left()
//!                 .map(Sentence::Question);
//!
//!             sentence(pos)
//!                 .or_from(pos)
//!                 .or(exclamation)
//!                 .or(question).into()
//!         }
//!         fn word(&self, pos: usize) -> Step<'a,Item<'a>> {
//!             token!(self.inner.token(pos) =>
//!                      Token::Word(v) => Item::Word(v),
//!                      Token::Comma => Item::Comma
//!             )
//!         }
//!         fn text(&self) -> Result<Vec<Sentence<'a>>, ParseError<'a>> {
//!             self.inner.zero_or_more(0, |p| self.sentence(p)).into()
//!         }
//!     }
//!
//!
//!         let parser = Parser::new(r#"
//!             I have a strange addiction,
//!             It often sets off sparks!
//!             I really cannot seem to stop,
//!             Using exclamation marks!
//!             Anyone heard of the interrobang?
//!             The poem is for kids.
//!         "#);
//!
//!         let result = parser.text();
//!         println!("{:?}",result);
//!
//! ```
//!
//!

pub mod lexer;
pub mod step;
pub mod parser;
pub mod error;
pub mod test;
