use once_cell::sync::Lazy;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Keyword {
    Define,
    Extern,
    Const,
}

impl Keyword {
    const KEYWORD_MAP: Lazy<HashMap<String, Keyword>> = Lazy::new(|| {
        HashMap::from(
            [
                ("def", Keyword::Define),
                ("extern", Keyword::Extern),
                ("const", Keyword::Const),
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

#[derive(Debug, Clone)]
pub enum Symbol {
    Add,
    Substract,
    Multiply,
    Divide,

    LeftParenthese,
    RightParenthese,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
}

impl Symbol {
    const SYMBOL_MAP: Lazy<HashMap<char, Symbol>> = Lazy::new(|| {
        HashMap::from([
            ('+', Symbol::Add),
            ('-', Symbol::Substract),
            ('*', Symbol::Multiply),
            ('/', Symbol::Divide),
            ('(', Symbol::LeftParenthese),
            (')', Symbol::RightParenthese),
            ('[', Symbol::LeftBracket),
            (']', Symbol::RightBracket),
            ('{', Symbol::LeftBrace),
            ('}', Symbol::RightBrace),
        ])
    });

    pub fn validate(c: char) -> bool {
        Self::SYMBOL_MAP.contains_key(&c)
    }

    pub fn from(c: char) -> Option<Symbol> {
        Self::SYMBOL_MAP.get(&c).map(Symbol::clone)
    }
}

#[derive(Debug)]
pub enum Token {
    Identifier(String),
    IntLiteral(i32),
    FloatLiteral(f64),

    Keyword(Keyword),

    Symbol(Symbol),
}
