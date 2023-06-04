use std::fmt::format;
use logos::Logos;
use crate::error::ParseError;
use crate::token;

#[cfg(windows)]
const LINE_SEP: &'static str = "\r\n";
#[cfg(not(windows))]
const LINE_SEP: &'static str = "\n";

fn create_marker(pos: usize) -> String {
    let mut del = String::new();
    del.push_str(LINE_SEP);
    for _ in 0..pos {
        del.push_str(" ");
    }
    del.push_str("^");
    del.push_str(LINE_SEP);
    del
}

/// A simple wrapper for `Logos` that works with &str
pub(crate) struct LexIt<'a, T>
    where
        T: Logos<'a, Source=str>,
{
    tokens_str: Vec<&'a str>,
    pub(crate) tokens: Vec<T>,
}

impl<'a, T> LexIt<'a, T>
    where
        T: Logos<'a, Source=str> + PartialEq,
{
    /// construct a new Lexer or return an error
    pub(crate) fn new(source: &'a str) -> Result<Self, ParseError<'a>>
        where
            T::Extras: Default,
    {
        let mut delegate = T::lexer(source);
        let mut tokens = vec![];
        let mut tokens_str = vec![];

        while let Some(t) = delegate.next() {
            match t {
                Ok(tok) => {
                    tokens.push(tok);
                    tokens_str.push(delegate.slice());
                }
                Err(_) => {
                    return Err(ParseError::BadToken(delegate.slice(), delegate.span()));
                }
            }

        }

        Ok(LexIt { tokens_str, tokens })
    }

    pub(crate) fn token(&self, pos: usize) -> Result<(&T, usize), ParseError<'a>> {
        match self.tokens.get(pos) {
            None => Err(ParseError::ReachedEOF(pos)),
            Some(t) => Ok((t, pos)),
        }
    }

    pub(crate) fn env(&self, pos: usize) -> String {
        let mut str = String::new();
        let max = self.tokens_str.len() - 1;

        let left = pos - 3;
        let right = pos + 3;

        let left = if left <= 0 { 0 } else { left };
        let right = if right >= max { max } else { right };

        let mut idx = left;
        let mut line = 0;
        let mut pos_in_line = 0;
        for p in &self.tokens_str[left..=right] {
            str.push_str(*p);
            if idx == pos {
                line = str.lines().count() - 1;
                pos_in_line = str.lines().last().map(|s| s.len() - p.len()).unwrap_or(0);
            }
            idx = idx + 1;
        }

        let mut final_str = String::new();

        for (i, s) in str.lines().into_iter().enumerate() {
            final_str.push_str(s);
            if i == line {
                final_str.push_str(create_marker(pos_in_line).as_str());
            }
        }

        final_str
    }

    pub(crate) fn len(&self) -> usize {
        self.tokens.len()
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{EmptyToken, Parsit};
    use crate::token;
    use logos::Logos;
    use crate::step::Step;

    #[derive(Logos, PartialEq)]
    pub enum T<'a> {
        #[regex(r"[a-zA-Z-]+")]
        Word(&'a str),

        #[token("|")]
        Del,
    }

    #[test]
    fn test() {
        let pit: Parsit<T> = Parsit::new("abc|bcd|a|b|x").unwrap();
        let x = pit.env::<EmptyToken>(&Step::Fail(9));
        println!("{}", x)
    }
}