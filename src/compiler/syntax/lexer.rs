use super::{
    err::{LexError, LexErrorKind},
    token::{Keyword, Symbol, Token},
};

struct CodeReader {
    code: Vec<char>,
    position: usize,
}

impl CodeReader {
    fn new(code: &str) -> CodeReader {
        let code: Vec<_> = code.chars().collect();
        CodeReader { code, position: 0 }
    }

    fn peek(&self) -> Option<char> {
        if self.position >= self.code.len() {
            return None;
        }
        Some(self.code[self.position])
    }

    fn forward(&mut self) {
        self.position += 1;
    }

    fn skip_spaces(&mut self) {
        while self.peek().is_some_and(|c| c.is_ascii_whitespace()) {
            self.forward();
        }
    }
}

pub struct Lexer {
    reader: CodeReader,
    next_token: Option<Token>,
}

impl Lexer {
    #[must_use]
    pub fn new(code: &str) -> Lexer {
        let mut lexer = Lexer {
            reader: CodeReader::new(code),
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
            if !matches!(c, 'a'..='z' | 'A'..='Z' | '_') {
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

    fn digest_symbol(&mut self) -> Result<Token, LexError> {
        if let Some(c) = self.reader.peek() {
            self.reader.forward();
            let symbol = match c {
                '+' => Symbol::Add,
                '-' => Symbol::Subtract,
                '*' => Symbol::Multiply,
                '/' => Symbol::Divide,
                '=' => {
                    if let Some('=') = self.reader.peek() {
                        self.reader.forward();
                        Symbol::Equal
                    } else {
                        Symbol::Assign
                    }
                }
                '!' => {
                    if let Some('=') = self.reader.peek() {
                        self.reader.forward();
                        Symbol::NotEqual
                    } else {
                        Symbol::LogicalNot
                    }
                }
                '<' => {
                    if let Some('=') = self.reader.peek() {
                        self.reader.forward();
                        Symbol::LessThanEqual
                    } else {
                        Symbol::LessThan
                    }
                }
                '>' => {
                    if let Some('=') = self.reader.peek() {
                        self.reader.forward();
                        Symbol::GreaterThanEqual
                    } else {
                        Symbol::GreaterThan
                    }
                }
                '&' => {
                    if let Some('&') = self.reader.peek() {
                        self.reader.forward();
                        Symbol::LogicalAnd
                    } else {
                        Symbol::BitAnd
                    }
                }
                '|' => {
                    if let Some('|') = self.reader.peek() {
                        self.reader.forward();
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
