use crate::compiler::lexis::token::Keyword;
use crate::compiler::lexis::token::Symbol;
use crate::compiler::lexis::token::Token;
use crate::util::reportable_error::Reportable;

pub enum ParseErrorKind {
    MissingIdentifier,
    MissingKeyword(Keyword),
    MissingSymbol(Symbol),

    UnexpectedEOF,
    UnexpectedToken(Token),

    UnknownType(String),
    UnknownPackage,
}

pub struct ParseError {
    pub kind: ParseErrorKind,
}

impl Reportable for ParseError {
    fn report(&self) -> ! {
        let message = match &self.kind {
            ParseErrorKind::MissingIdentifier => "failed to expect an identifier".to_string(),
            ParseErrorKind::MissingKeyword(keyword) => {
                format!("missing expected keyword: {keyword:?}")
            }
            ParseErrorKind::MissingSymbol(symbol) => {
                format!("missing expected symbol: {symbol:?}")
            }
            ParseErrorKind::UnexpectedEOF => "encountering unexpected EOF".to_string(),
            ParseErrorKind::UnexpectedToken(token) => {
                format!("encountering unexpected token: {token:?}")
            }
            ParseErrorKind::UnknownType(unknown_type) => {
                format!("encountering unknown type: {unknown_type}")
            }
            ParseErrorKind::UnknownPackage => "unknown package.".to_string(),
        };
        panic!("{message}")
    }
}
