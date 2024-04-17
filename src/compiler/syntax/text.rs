pub struct Reader {
    code: Vec<char>,
    position: usize,
}

impl Reader {
    pub fn new(code: &str) -> Reader {
        let code: Vec<_> = code.chars().collect();
        Reader { code, position: 0 }
    }

    pub fn peek(&self) -> Option<char> {
        if self.position >= self.code.len() {
            return None;
        }
        Some(self.code[self.position])
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
