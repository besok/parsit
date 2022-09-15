use std::ops::Range;
/// Parsing error.
#[derive(Debug, Clone)]
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
}
