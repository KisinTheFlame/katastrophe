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
            Err(e) => e.report_and_panic(),
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
            Err(e) => e.report_and_panic(),
        }
    }

    fn pump_token(&mut self) -> Result<(), LexError> {
        self.reader.skip_spaces();
        if let Some(token) = self.reader.peek() {
            let token = match token {
                '0'..='9' => self.digest_number()?,
                'a'..='z' | 'A'..='Z' | '_' => self.digest_identifier_or_keyword()?,
                _ => self.digest_symbol()?,
            };
            self.next_token = Some(token);
        } else {
            self.next_token = None;
        }
        Ok(())
    }

    fn digest_number(&mut self) -> Result<Token, LexError> {
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
                }),
            }
        } else {
            match number.parse::<i32>() {
                Ok(x) => Ok(Token::IntLiteral(x)),
                Err(_) => Err(LexError {
                    kind: LexErrorKind::IllegalIntegerLiteral(number),
                }),
            }
        }
    }

    fn digest_identifier_or_keyword(&mut self) -> Result<Token, LexError> {
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
        let token = if Keyword::validate(&identifier) {
            Token::Keyword(Keyword::from(&identifier).unwrap())
        } else {
            Token::Identifier(identifier)
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
        if let Some(c) = self.reader.peek() {
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
                c => {
                    return Err(LexError {
                        kind: LexErrorKind::UnexpectedCharacter(c),
                    });
                }
            };
            Ok(Token::Symbol(symbol))
        } else {
            Err(LexError {
                kind: LexErrorKind::UnexpectedEOF,
            })
        }
    }
}
