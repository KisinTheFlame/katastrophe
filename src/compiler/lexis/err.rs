use crate::util::reportable_error::Reportable;

pub enum LexErrorKind {
    IllegalIntegerLiteral(String),
    IllegalFloatLiteral(String),
    IllegalEscapeChar(char),

    UnexpectedCharacter(char),
    UnexpectedEOF,
}

pub struct LexError {
    pub kind: LexErrorKind,
}

impl Reportable for LexError {
    fn report(&self) -> ! {
        let message = match self.kind {
            LexErrorKind::IllegalIntegerLiteral(ref s)
            | LexErrorKind::IllegalFloatLiteral(ref s) => {
                format!("encountering illegal literal: {s}")
            }
            LexErrorKind::IllegalEscapeChar(c) => format!("illegal escape char: {c}"),
            LexErrorKind::UnexpectedCharacter(c) => {
                format!("encountering unexpected character: {c}")
            }
            LexErrorKind::UnexpectedEOF => "code should not end here".to_string(),
        };
        panic!("{message}")
    }
}
