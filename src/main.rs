use std::fs;
use std::process;

use compiler::syntax::lexer::Lexer;

pub mod compiler;

fn main() {
    let mut lexer = Lexer::new(match fs::read_to_string("test.txt") {
        Ok(code) => code,
        Err(_) => {
            println!("faced fatal error when reading file");
            process::exit(1);
        }
    });
    while let Some(token) = lexer.peek() {
        println!("{:?}", token);
        lexer.next();
    }
}
