use crate::{compiler::err::CompileError, sys_error};

use super::{
    err::{LexError, LexErrorKind},
    text::Reader,
    token::{Keyword, Symbol, Token},
};

pub struct Lexer {
    reader: Reader,
    next_token: Option<Token>,
}

impl Lexer {
    #[must_use]
    pub fn new(code: &str) -> Lexer {
        let mut lexer = Lexer {
            reader: Reader::new(code),
            next_token: None,
        };
        match lexer.pump_token() {
            Ok(()) => lexer,
            Err(e) => e.report(),
        }
    }

    pub fn peek(&mut self) -> Option<&Token> {
        self.next_token.as_ref()
    }

    pub fn next(&mut self) {
        if self.next_token.is_none() {
            return;
        }
        match self.pump_token() {
            Ok(()) => (),
            Err(e) => e.report(),
        }
    }

    fn pump_token(&mut self) -> Result<(), CompileError> {
        self.reader.skip_spaces();
        self.next_token = self.digest_token()?;
        Ok(())
    }

    fn digest_token(&mut self) -> Result<Option<Token>, CompileError> {
        self.reader.skip_spaces();
        if let Some(c) = self.reader.peek() {
            let token = match c {
                '0'..='9' => Some(self.digest_number()?),
                'a'..='z' | 'A'..='Z' | '_' => Some(self.digest_identifier_or_keyword_or_bool()?),
                '#' => {
                    self.digest_comment()?;
                    self.digest_token()?
                }
                _ => Some(self.digest_symbol()?),
            };
            Ok(token)
        } else {
            Ok(None)
        }
    }

    fn digest_comment(&mut self) -> Result<(), CompileError> {
        if let Some('#') = self.reader.peek() {
            self.reader.forward();
        } else {
            return sys_error!("must be a #");
        }
        while let Some(c) = self.reader.peek() {
            if c == '\n' {
                break;
            }
            self.reader.forward();
        }
        Ok(())
    }

    fn digest_number(&mut self) -> Result<Token, CompileError> {
        let mut number = String::new();
        while let Some(c) = self.reader.peek() {
            if !(c.is_numeric() || c == '.') {
                break;
            }
            number.push(c);
            self.reader.forward();
        }
        if number.contains('.') {
            match number.parse::<f64>() {
                Ok(x) => Ok(Token::FloatLiteral(x)),
                Err(_) => Err(LexError {
                    kind: LexErrorKind::IllegalFloatLiteral(number),
                }
                .into()),
            }
        } else {
            match number.parse::<i32>() {
                Ok(x) => Ok(Token::IntLiteral(x)),
                Err(_) => Err(LexError {
                    kind: LexErrorKind::IllegalIntegerLiteral(number),
                }
                .into()),
            }
        }
    }

    fn digest_identifier_or_keyword_or_bool(&mut self) -> Result<Token, LexError> {
        let mut identifier = String::new();
        while let Some(c) = self.reader.peek() {
            if c.is_ascii_whitespace() {
                if identifier.is_empty() {
                    return Err(LexError {
                        kind: LexErrorKind::UnexpectedCharacter(c),
                    });
                }
                break;
            }
            if !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_') {
                break;
            }
            identifier.push(c);
            self.reader.forward();
        }
        if identifier.is_empty() {
            return Err(LexError {
                kind: LexErrorKind::UnexpectedEOF,
            });
        }
        let token = match identifier.as_str() {
            "true" => Token::BoolLiteral(true),
            "false" => Token::BoolLiteral(false),
            str => {
                if Keyword::validate(str) {
                    Token::Keyword(Keyword::from(str).unwrap())
                } else {
                    Token::Identifier(identifier)
                }
            }
        };
        Ok(token)
    }

    fn expect(&mut self, expected: char) -> bool {
        if let Some(c) = self.reader.peek() {
            if c == expected {
                self.reader.forward();
                return true;
            }
        }
        false
    }

    fn digest_symbol(&mut self) -> Result<Token, LexError> {
        let c = self.reader.peek().unwrap();
        self.reader.forward();
        let symbol = match c {
            '+' => Symbol::Add,
            '-' => {
                if self.expect('>') {
                    Symbol::Arrow
                } else {
                    Symbol::Subtract
                }
            }
            '*' => Symbol::Multiply,
            '/' => Symbol::Divide,
            '=' => {
                if self.expect('=') {
                    Symbol::Equal
                } else {
                    Symbol::Assign
                }
            }
            '!' => {
                if self.expect('=') {
                    Symbol::NotEqual
                } else {
                    Symbol::LogicalNot
                }
            }
            '<' => {
                if self.expect('=') {
                    Symbol::LessThanEqual
                } else {
                    Symbol::LessThan
                }
            }
            '>' => {
                if self.expect('=') {
                    Symbol::GreaterThanEqual
                } else {
                    Symbol::GreaterThan
                }
            }
            '&' => {
                if self.expect('&') {
                    Symbol::LogicalAnd
                } else {
                    Symbol::BitAnd
                }
            }
            '|' => {
                if self.expect('|') {
                    Symbol::LogicalOr
                } else {
                    Symbol::BitOr
                }
            }
            '~' => Symbol::BitNot,
            '(' => Symbol::LeftParentheses,
            ')' => Symbol::RightParentheses,
            '[' => Symbol::LeftBracket,
            ']' => Symbol::RightBracket,
            '{' => Symbol::LeftBrace,
            '}' => Symbol::RightBrace,
            ',' => Symbol::Comma,
            ';' => Symbol::Semicolon,
            ':' => {
                if self.expect(':') {
                    Symbol::DoubleColon
                } else {
                    return Err(LexError {
                        kind: LexErrorKind::UnexpectedCharacter(':'),
                    });
                }
            }
            c => {
                return Err(LexError {
                    kind: LexErrorKind::UnexpectedCharacter(c),
                });
            }
        };
        Ok(Token::Symbol(symbol))
    }
}
