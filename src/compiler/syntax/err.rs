use crate::{compiler::err::InnerCompilerError, util::reportable_error::ReportableError};

use super::token::{Keyword, Symbol, Token};

pub enum LexErrorKind {
    Unknown,
    InternalMess(&'static str),

    IllegalIntegerLiteral(String),
    IllegalFloatLiteral(String),

    UnexpectedCharacter(char),
    UnexpectedEOF,
}

pub struct LexError {
    pub kind: LexErrorKind,
}

impl ReportableError for LexError {
    fn report(&self) -> ! {
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

pub enum ParseErrorKind {
    Unknown,
    InternalMess(&'static str),

    MissingIdentifier,
    MissingKeyword(Keyword),
    MissingSymbol(Symbol),

    UnexpectedEOF,
    UnexpectedToken(Token),

    UnknownType(String),
}

pub struct ParseError {
    pub kind: ParseErrorKind,
}

impl InnerCompilerError for ParseError {}

impl ReportableError for ParseError {
    fn report(&self) -> ! {
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
            ParseErrorKind::UnknownType(ref unknown_type) => {
                format!("encountering unknown type: {unknown_type}")
            }
        };
        panic!("{message}")
    }
}
