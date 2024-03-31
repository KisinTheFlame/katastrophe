use super::{
    error::{LexError, LexErrorKind},
    token::{Keyword, Symbol, Token},
};

struct CodeReader {
    code: Vec<char>,
    position: usize,
}

impl CodeReader {
    fn new(code: String) -> CodeReader {
        let code: Vec<_> = code.chars().into_iter().collect();
        return CodeReader { code, position: 0 };
    }

    fn peek(&self) -> Option<char> {
        if self.position >= self.code.len() {
            return None;
        }
        return Some(self.code[self.position]);
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
    pub fn new(code: String) -> Lexer {
        let mut lexer = Lexer {
            reader: CodeReader::new(code),
            next_token: None,
        };
        match lexer.pump_token() {
            Ok(_) => return lexer,
            Err(e) => panic!("{:?}", e),
        };
    }

    pub fn peek(&mut self) -> &Option<Token> {
        return &self.next_token;
    }

    pub fn next(&mut self) {
        if self.next_token.is_none() {
            return;
        }
        match self.pump_token() {
            Ok(_) => (),
            Err(e) => panic!("{:#?}", e),
        }
    }

    fn pump_token(&mut self) -> Result<(), LexError> {
        self.reader.skip_spaces();
        let token = match self.reader.peek() {
            Some('0'..='9') => self.digest_number()?,
            Some('a'..='z' | 'A'..='Z' | '_') => self.digest_identifier_or_keyword()?,
            Some(c) => {
                if Symbol::validate(c) {
                    self.digest_symbol()?
                } else {
                    return Err(LexError {
                        kind: LexErrorKind::UnexpectedCharacter(c),
                    });
                }
            }
            None => {
                self.next_token = None;
                return Ok(());
            }
        };
        self.next_token = Some(token);
        return Ok(());
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
        if !number.contains('.') {
            match number.parse::<i32>() {
                Ok(x) => return Ok(Token::IntLiteral(x)),
                Err(_) => {
                    return Err(LexError {
                        kind: LexErrorKind::IllegalIntegerLiteral(number),
                    })
                }
            };
        } else {
            match number.parse::<f64>() {
                Ok(x) => return Ok(Token::FloatLiteral(x)),
                Err(_) => {
                    return Err(LexError {
                        kind: LexErrorKind::IllegalFloatLiteral(number),
                    })
                }
            };
        }
    }

    fn digest_identifier_or_keyword(&mut self) -> Result<Token, LexError> {
        let mut identifier = String::new();
        while let Some(c) = self.reader.peek() {
            if !c.is_ascii_alphanumeric() && c != '_' {
                break;
            }
            identifier.push(c);
            self.reader.forward();
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
            let symbol = match Symbol::from(c) {
                Some(symbol) => symbol,
                None => {
                    return Err(LexError {
                        kind: LexErrorKind::UnexpectedCharacter(c),
                    })
                }
            };
            self.reader.forward();
            Ok(Token::Symbol(symbol))
        } else {
            return Err(LexError {
                kind: LexErrorKind::UnexpectedEOF,
            });
        }
    }
}
