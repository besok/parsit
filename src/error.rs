use std::ops::Range;

/// Parsing error.
#[derive(Debug, Clone, PartialEq)]
pub enum ParseError<'a> {
    /// The token is bad and apparently the error is on the level of lexing
    BadToken(&'a str, Range<usize>),
    /// When the validation is not working
    ///
    /// # Examples
    ///
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
    ///
    ///
    FailedOnValidation(&'a str, usize),
    /// When the last token is fail. It happens when the backtracking does not have a positive variant.
    FinishedOnFail,
    /// When the token stream is empty but the parser expects other tokens
    ReachedEOF(usize),
    /// When the token stream si not empty and parser does not expect anything.
    UnreachedEOF(usize),
    /// External error usually manually created
    ExternalError(String, usize),
}

impl<'a> ToString for ParseError<'a> {
    fn to_string(&self) -> String {
        match self {
            ParseError::BadToken(a, b) => format!("bad token {:?}, range: ([{:?}..{:?}])", a, b.start, b.end),
            ParseError::FailedOnValidation(a, b) =>format!("validation failed on pos {:?}, mes:{:?} ",b,a),
            ParseError::FinishedOnFail => format!("it finished parsing on the fail positon"),
            ParseError::ReachedEOF(p) => format!("it ends parsing on {:?}",p),
            ParseError::UnreachedEOF(p) => format!("it ends earlier on {:?}",p),
            ParseError::ExternalError(a, b) => format!("external error: {:?} on {:?}",a,b),
        }
    }
}
