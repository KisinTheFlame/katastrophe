use super::token::{Keyword, Symbol, Token};

#[derive(Debug)]
pub enum LexErrorKind {
    Unknown,
    InternalMess(&'static str),

    IllegalIntegerLiteral(String),
    IllegalFloatLiteral(String),

    UnexpectedCharacter(char),
    UnexpectedEOF,
}

#[derive(Debug)]
pub struct LexError {
    pub kind: LexErrorKind,
}

impl LexError {
    /// # Panics
    pub fn report_and_panic(&self) -> ! {
        let message = match self.kind {
            LexErrorKind::Unknown => "encountering unknown error".to_string(),
            LexErrorKind::InternalMess(message) => message.to_string(),
            LexErrorKind::IllegalIntegerLiteral(ref s)
            | LexErrorKind::IllegalFloatLiteral(ref s) => {
                format!("encountering illegal literal: {s}")
            }
            LexErrorKind::UnexpectedCharacter(c) => {
                format!("encountering unexpected character: {c}")
            }
            LexErrorKind::UnexpectedEOF => "code should not end here".to_string(),
        };
        panic!("{message}")
    }
}

#[derive(Debug)]
pub enum ParseErrorKind {
    Unknown,
    InternalMess(&'static str),

    MissingIdentifier,
    MissingKeyword(Keyword),
    MissingSymbol(Symbol),

    UnexpectedEOF,
    UnexpectedToken(Token),
}

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
}

impl ParseError {
    /// # Panics
    pub fn report_and_panic(&self) -> ! {
        let message = match self.kind {
            ParseErrorKind::Unknown => "encountering unknown error".to_string(),
            ParseErrorKind::InternalMess(message) => message.to_string(),
            ParseErrorKind::MissingIdentifier => "failed to expect an identifier".to_string(),
            ParseErrorKind::MissingKeyword(ref keyword) => {
                format!("missing expected keyword: {keyword:?}")
            }
            ParseErrorKind::MissingSymbol(ref symbol) => {
                format!("missing expected symbol: {symbol:?}")
            }
            ParseErrorKind::UnexpectedEOF => "encountering unexpected EOF".to_string(),
            ParseErrorKind::UnexpectedToken(ref token) => {
                format!("encountering unexpected token: {token:?}")
            }
        };
        panic!("{message}")
    }
}
