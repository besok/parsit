use std::fmt::format;
use logos::Logos;
use crate::error::ParseError;
use crate::token;

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
            if t == T::ERROR {
                return Err(ParseError::BadToken(delegate.slice(), delegate.span()));
            } else {
                tokens.push(t);
                tokens_str.push(delegate.slice());
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
        let l = self.tokens_str.len();

        let left_border = if pos >= 2 { pos - 2 } else if pos >= 1 { pos - 1 } else { pos };
        let right_border = if pos + 2 < l { pos + 2 } else if pos + 1 < l { pos + 1 } else { pos };
        let mut idx = left_border;
        for p in &self.tokens_str[left_border..=right_border] {
            if idx == pos {
                str.push_str(format!(" >>{}<< ", *p).as_str())
            } else { str.push_str(*p) }
            idx = idx + 1;
        }
        str
    }

    pub(crate) fn len(&self) -> usize {
        self.tokens.len()
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ParseIt;
    use crate::token;
    use logos::Logos;

    #[derive(Logos, PartialEq)]
    pub enum T<'a> {
        #[regex(r"[a-zA-Z-]+")]
        Word(&'a str),

        #[token("|")]
        Del,

        #[error]
        Error,
    }

    #[test]
    fn test() {
        let pit: ParseIt<T> = ParseIt::new("abc|bcd|a|b|x").unwrap();
        let x = pit.env(1);
        println!("{}", x)
    }
}