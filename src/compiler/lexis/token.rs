use once_cell::sync::Lazy;
use std::collections::HashMap;

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
            ("struct", Keyword::Struct),
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
