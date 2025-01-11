use crate::compiler::syntax::ast::crumb::Identifier;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    Define,
    Using,
    Return,
    Let,
    If,
    Else,
    While,
    As,
    Mut,
    Builtin,
    Struct,
}

impl Keyword {
    #[must_use]
    pub fn of(s: &str) -> Option<Keyword> {
        match s {
            "def" => Some(Keyword::Define),
            "using" => Some(Keyword::Using),
            "return" => Some(Keyword::Return),
            "let" => Some(Keyword::Let),
            "if" => Some(Keyword::If),
            "else" => Some(Keyword::Else),
            "while" => Some(Keyword::While),
            "as" => Some(Keyword::As),
            "mut" => Some(Keyword::Mut),
            "builtin" => Some(Keyword::Builtin),
            "struct" => Some(Keyword::Struct),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Symbol {
    Add,
    Subtract,
    Multiply,
    Divide,

    LogicalAnd,
    LogicalOr,
    LogicalNot,

    BitAnd,
    BitOr,
    BitNot,

    LeftShift,
    RightShift,

    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,

    Assign,

    LeftParentheses,
    RightParentheses,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,

    Comma,
    Colon,
    Semicolon,
    DoubleColon,
    Dot,

    Arrow,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(Identifier),
    IntLiteral(i32),
    FloatLiteral(f64),
    CharLiteral(char),
    BoolLiteral(bool),

    Keyword(Keyword),
    Symbol(Symbol),

    Spawning(Identifier),
}
