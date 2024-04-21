use crate::util::reportable_error::ReportableError;

pub enum LexErrorKind {
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
