use std::fs;
use std::process;

use compiler::syntax::parser::Parser;

pub mod compiler;

fn main() {
    let code = match fs::read_to_string("test.txt") {
        Ok(code) => code,
        Err(_) => {
            println!("encountering fatal error when reading file");
            process::exit(1);
        }
    };
    let mut parser = Parser::new(code);
    match parser.parse_program() {
        Ok(program) => println!("{:#?}", program),
        Err(e) => println!("{:?}", e),
    }
}
