use logos::Logos;
use crate::error::ParseError;

/// A simple wrapper for `Logos` that works with &str
pub(crate) struct LexIt<'a, T>
    where
        T: Logos<'a, Source = str>,
{
    source: &'a str,
    tokens: Vec<T>,
}

impl<'a, T> LexIt<'a, T>
    where
        T: Logos<'a, Source = str> + PartialEq,
{
    /// construct a new Lexer or return an error
    pub(crate) fn new(source: &'a str) -> Result<Self, ParseError<'a>>
        where
            T::Extras: Default,
    {
        let mut delegate = T::lexer(source);
        let mut tokens = vec![];

        while let Some(t) = delegate.next() {
            if t == T::ERROR {
                return Err(ParseError::BadToken(delegate.slice(), delegate.span()))
            }else{
                tokens.push(t);
            }

        }

        Ok(LexIt { source, tokens })
    }

    pub(crate) fn token(&self, pos: usize) -> Result<(&T, usize), ParseError<'a>> {
        match self.tokens.get(pos) {
            None => Err(ParseError::ReachedEOF(pos)),
            Some(t) => Ok((t, pos)),
        }
    }
    pub(crate) fn len(&self) -> usize {
        self.tokens.len()
    }
}