#[derive(Debug)]
pub struct LexError {
    pub kind: LexErrorKind,
}

#[derive(Debug)]
pub enum LexErrorKind {
    Unknown,
    InternalMess(&'static str),

    IllegalIntegerLiteral(String),
    IllegalFloatLiteral(String),

    UnexpectedCharacter(char),
    UnexpectedEOF,
}
