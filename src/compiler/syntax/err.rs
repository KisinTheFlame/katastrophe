use crate::{
    compiler::{
        err::InnerCompilerError,
        lexis::token::{Keyword, Symbol, Token},
    },
    util::reportable_error::ReportableError,
};

pub enum ParseErrorKind {
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
