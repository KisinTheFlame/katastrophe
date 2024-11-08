use once_cell::sync::Lazy;
use std::collections::HashMap;

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
}

static KEYWORD_MAP: Lazy<HashMap<&'static str, Keyword>> = Lazy::new(|| {
    HashMap::from(
        [
            ("def", Keyword::Define),
            ("using", Keyword::Using),
            ("return", Keyword::Return),
            ("let", Keyword::Let),
            ("if", Keyword::If),
            ("else", Keyword::Else),
            ("while", Keyword::While),
            ("as", Keyword::As),
            ("mut", Keyword::Mut),
            ("builtin", Keyword::Builtin),
        ]
        .map(|(s, k)| (s, k)),
    )
});

impl Keyword {
    pub fn validate(s: &str) -> bool {
        KEYWORD_MAP.contains_key(s)
    }

    pub fn from(s: &str) -> Option<Keyword> {
        KEYWORD_MAP.get(s).copied()
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
    Semicolon,
    DoubleColon,

    Arrow,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    IntLiteral(i32),
    FloatLiteral(f64),
    CharLiteral(char),
    BoolLiteral(bool),

    Keyword(Keyword),
    Symbol(Symbol),
}
