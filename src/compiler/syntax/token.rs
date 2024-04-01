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

    LeftParentheses,
    RightParentheses,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,

    Comma,
    Semicolon,
}

impl Symbol {
    const SYMBOL_MAP: Lazy<HashMap<char, Symbol>> = Lazy::new(|| {
        HashMap::from([
            ('+', Symbol::Add),
            ('-', Symbol::Subtract),
            ('*', Symbol::Multiply),
            ('/', Symbol::Divide),
            ('(', Symbol::LeftParentheses),
            (')', Symbol::RightParentheses),
            ('[', Symbol::LeftBracket),
            (']', Symbol::RightBracket),
            ('{', Symbol::LeftBrace),
            ('}', Symbol::RightBrace),
            (',', Symbol::Comma),
            (';', Symbol::Semicolon),
        ])
    });

    pub fn validate(c: char) -> bool {
        Self::SYMBOL_MAP.contains_key(&c)
    }

    pub fn from(c: char) -> Option<Symbol> {
        Self::SYMBOL_MAP.get(&c).map(Symbol::clone)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    IntLiteral(i32),
    FloatLiteral(f64),

    Keyword(Keyword),

    Symbol(Symbol),
}
