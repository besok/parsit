### Description
This library provides a very simple and lightweight parser (recursive descendant ll(1)) to combine and express 
a given grammar.

The library uses [Logos](https://github.com/maciejhirsz/logos) as a lexical analyzer and tokenizer.

### The steps 

#### Create a set of tokens using Logos
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
                .or(question).into()
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
        println!("{:?}",result);
    }
```



