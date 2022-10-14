# ParseIt

### Description

This library provides a very simple and lightweight parser (recursive descendant ll(1)) to combine and express a
grammar.

The library uses [Logos](https://github.com/maciejhirsz/logos) as a lexical analyzer and tokenizer.

### The premise

This library major incentives were:

- lightweight : very small and does not require a deep dive
- transparency : literally 3 structs with a handful of methods
- speed : good speed (with a gratitude to [Logos](https://github.com/maciejhirsz/logos))

### The steps to implement

#### Create a set of tokens using Logos

#### Add logos to dependency

```cargo
    logos = "*"
```

```rust
use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
enum Token {
    // Tokens can be literal strings, of any length.
    #[token("fast")]
    Fast,

    #[token(".")]
    Period,

    // Or regular expressions.
    #[regex("[a-zA-Z]+")]
    Text,

    // Logos requires one token variant to handle errors,
    // it can be named anything you wish.
    #[error]
    // We can also use this variant to define whitespace,
    // or any other matches we wish to skip.
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}
```

#### Create a parser that will be able to parse the given set of tokens

The library provides `ParseIt<'a,T>` instance that encompasses a set of tokens and auxiliary methods

```rust

struct Parser<'a> {
    inner: ParseIt<'a, Token<'a>>,
}

```

#### Implement a parsing functions using `ParseIt` instance and auxiliary methods from the `Step`

The helpers:

- the macros token! that alleviates comparing and matching single tokens
- methods `then`, `then_zip` and others from `Step`
- methods `one_or_more`, `zero_or_more` from `ParseIt`

#### Transform the result into `Result<Structure, ParserError<'a>>`

```rust
      fn text(&self, pos: usize) -> Result<Vec<Sentence<'a>>, ParseError<'a>> {
    self.inner.zero_or_more(pos, |p| self.sentence(p)).into()
}
```

### Complete example

```rust
  use crate::parser::ParseIt;
use crate::token;
use crate::step::Step;
use crate::parser::EmptyToken;
use crate::error::ParseError;
use logos::Logos;


#[derive(Logos, Debug, Copy, Clone, PartialEq)]
pub enum Token<'a> {
    #[regex(r"[a-zA-Z-]+")]
    Word(&'a str),

    #[token(",")]
    Comma,
    #[token(".")]
    Dot,

    #[token("!")]
    Bang,
    #[token("?")]
    Question,

    #[regex(r"[ \t\r\n\u000C\f]+", logos::skip)]
    Whitespace,
    #[error]
    Error,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum Item<'a> {
    Word(&'a str),
    Comma,
}

#[derive(Debug, Clone, PartialEq)]
enum Sentence<'a> {
    Sentence(Vec<Item<'a>>),
    Question(Vec<Item<'a>>),
    Exclamation(Vec<Item<'a>>),
}

struct Parser<'a> {
    inner: ParseIt<'a, Token<'a>>,
}

impl<'a> Parser<'a> {
    fn new(text: &'a str) -> Parser<'a> {
        let delegate: ParseIt<Token> = ParseIt::new(text).unwrap();
        Parser { inner: delegate }
    }

    fn sentence(&self, pos: usize) -> Step<'a, Sentence<'a>> {
        let items = |p| self.inner.one_or_more(p, |p| self.word(p));

        let sentence = |p| items(p)
            .then_zip(|p| token!(self.inner.token(p) => Token::Dot))
            .take_left()
            .map(Sentence::Sentence);

        let exclamation = |p| items(p)
            .then_zip(|p| token!(self.inner.token(p) => Token::Bang))
            .take_left()
            .map(Sentence::Exclamation);
        let question = |p| items(p)
            .then_zip(|p| token!(self.inner.token(p) => Token::Question))
            .take_left()
            .map(Sentence::Question);

        sentence(pos)
            .or_from(pos)
            .or(exclamation)
            .or(question)
            .into()
    }
    fn word(&self, pos: usize) -> Step<'a, Item<'a>> {
        token!(self.inner.token(pos) =>
                     Token::Word(v) => Item::Word(v),
                     Token::Comma => Item::Comma
            )
    }
    fn text(&self, pos: usize) -> Result<Vec<Sentence<'a>>, ParseError<'a>> {
        self.inner.zero_or_more(pos, |p| self.sentence(p)).into()
    }
}


#[test]
fn test() {
    let parser = Parser::new(r#"
            I have a strange addiction,
            It often sets off sparks!
            I really cannot seem to stop,
            Using exclamation marks!
            Anyone heard of the interrobang?
            The poem is for kids.
        "#);

    let result = parser.text(0).unwrap();
    println!("{:?}", result);
}
```

### The base auxiliary methods

#### On parser

- `token` - gives a possibility to pull out a curren token
- `one_or_more` - gives a one or more semantic
- `zero_or_more` - gives a zero or more semantic
- `validate_eof` - ensure the parser reaches end of the input

#### Macros

- `token!` - parses the current token. In general, it is used the following `token!(p.token(pos) => T::Bang => "!")`
- `wrap!`  - implements a simple pattern in grammar like `left value right`, for instance `[1,2,3]` or `(a,b)`
    - can handle the default value like `wrap!(0 => left; value or default; right)`
    - can handle the option value like `wrap!(0 => left; value ?; right)`
- `seq!` - implements a simple pattern of sequence like `el sep el ... `, for instance `1,2,3`
    - can have a `,` at the end signaling the separator can be at the ned of the seq like `1,2,3 (,)?`

#### On step

##### To alternate

- `or` - gives an alternative in a horizon of one token
- `or_from` - gives a backtracking option

##### To combine

- `then` - gives a basic combination with a next rule omitting the current one
- `then_zip` - combines a current result and a next one into a pair
- `then_skip` - parses the next one but drops the result keeping only current one
- `then_or_none` -combines a next one in an option with a current one or return a none otherwise

##### To collect

- `take_left` - drops a right value from a pair
- `take_right` - drops a left value from a pair
- `merge` - merge a value into a list
- `to_map` - transforms a list of pairs into a map

##### To transform

- `or_val` - replaces a value with a default value if it is not presented
- `or_none` - replaces a value with a none if it is not presented

##### To work with value

- `ok` - transforms a value into an option
- `error` - transforms an error into an option
- `map` - transforms a value
- `combine` - combines a value with another value from a given step
- `validate` - validates a given value and transforms into an error if a validation failed

##### To print

- `print` - print a step
- `print_with` - print a step with a given prefix
- `print_as` - print a step with a transformation of value
- `print_with_as` - print a step with a transformation of value with a given prefix
- `parit.env` - Prints a position and env from the source text(with a radius 3 
  token )

### Testing

#### Lexer

To test a lexer there are methods from `crate::parsit::test::lexer_test::*` for service

```rust
 use logos::Logos;
use crate::parsit::test::lexer_test::*;

#[derive(Logos, Debug, PartialEq)]
pub enum T<'a> {
    #[regex(r"[a-zA-Z-]+")]
    Word(&'a str),

    #[token(",")]
    Comma,
    #[token(".")]
    Dot,

    #[token("!")]
    Bang,
    #[token("?")]
    Question,

    #[regex(r"[ \t\r\n]+", logos::skip)]
    Whitespace,
    #[error]
    Error,
}

#[test]
fn test() {
    expect::<T>("abc, bcs!", vec![T::Word("abc"), T::Comma, T::Word("bcs"), T::Bang]);
    expect_succeed::<T>("abc, bcs!");
    expect_failed::<T>("abc, bcs >> !");
    expect_failed_with::<T, _>("abc, bcs > !", |e| e.is_bad_token_on(">"));
}
```

#### Parser

To test a parser there are methods from `crate::parsit::test::parser_test::*` for service

```rust
use logos::Logos;
use crate::parsit::test::parser_test::fail;
use crate::parsit::test::parser_test::parsit;
use crate::parsit::token;
use crate::parsit::parser::ParseIt;
use crate::parsit::step::Step;

#[derive(Logos, Debug, PartialEq)]
pub enum T<'a> {
    #[regex(r"[a-zA-Z-]+")]
    Word(&'a str),

    #[token(",")]
    Comma,
    #[token(".")]
    Dot,

    #[token("!")]
    Bang,
    #[token("?")]
    Question,

    #[regex(r"[ \t\r\n]+", logos::skip)]
    Whitespace,
    #[error]
    Error,
}

#[test]
fn test_expect() {
    let p = parsit("abc!");
    let bang = |pos: usize| token!(p.token(pos) => T::Bang => "!");
    let word = |pos: usize| token!(p.token(pos) => T::Word(v) => *v);
    let step =
        word(0)
            .then_or_val_zip(bang, "")
            .map(|(a, b)| format!("{}{}", a, b));

    expect(step, "abc!".to_string());
}

#[test]
fn test_pos() {
    let p = parsit("abc!");
    let bang = |pos: usize| token!(p.token(pos) => T::Bang => "!");
    let word = |pos: usize| token!(p.token(pos) => T::Word(v) => v);
    let step = word(0).then_or_val_zip(bang, "");

    expect_pos(step, 2); // the next position to parse
}

#[test]
fn test_fail() {
    let p = parsit("abc?!");
    let bang = |pos: usize| token!(p.token(pos) => T::Bang => "!");
    let word = |pos: usize| token!(p.token(pos) => T::Word(v) => v);
    let step = word(0).then_zip(bang);

    fail(step);
}

#[test]
fn test_fail_on() {
    let p = parsit("abc?!");
    let bang = |pos: usize| token!(p.token(pos) => T::Bang => "!");
    let word = |pos: usize| token!(p.token(pos) => T::Word(v) => v);
    let step = word(0).then_zip(bang);

    fail_on(step, 1);
}



```