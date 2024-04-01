use once_cell::sync::Lazy;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Define,
    Import,
    Const,
    If,
    Else,
    While,
}

impl Keyword {
    const KEYWORD_MAP: Lazy<HashMap<String, Keyword>> = Lazy::new(|| {
        HashMap::from(
            [
                ("def", Keyword::Define),
                ("import", Keyword::Import),
                ("const", Keyword::Const),
                ("if", Keyword::If),
                ("else", Keyword::Else),
                ("while", Keyword::While),
            ]
            .map(|(s, k)| (s.to_string(), k)),
        )
    });

    pub fn validate(s: &String) -> bool {
        Self::KEYWORD_MAP.contains_key(s)
    }

    pub fn from(s: &String) -> Option<Keyword> {
        Self::KEYWORD_MAP.get(s).map(Keyword::clone)
    }
}

#[derive(Debug, Clone, PartialEq)]
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    IntLiteral(i32),
    FloatLiteral(f64),

    Keyword(Keyword),

    Symbol(Symbol),
}
