use crate::compiler::err::CompileError;

use super::err::LexError;
use super::err::LexErrorKind;

pub struct Reader {
    code: Vec<char>,
    position: usize,
}

impl Reader {
    #[must_use]
    pub fn new(code: &str) -> Reader {
        let code: Vec<_> = code.chars().collect();
        Reader { code, position: 0 }
    }

    #[must_use]
    pub fn peek(&self) -> Option<char> {
        if self.position >= self.code.len() {
            return None;
        }
        Some(self.code[self.position])
    }

    /// # Errors
    pub fn peek_unwrap(&self) -> Result<char, CompileError> {
        self.peek().ok_or(
            LexError {
                kind: LexErrorKind::UnexpectedEOF,
            }
            .into(),
        )
    }

    pub fn forward(&mut self) {
        self.position += 1;
    }

    pub fn skip_spaces(&mut self) {
        while self.peek().is_some_and(|c| c.is_ascii_whitespace()) {
            self.forward();
        }
    }
}
