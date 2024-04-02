use std::fs;
use std::process;

use compiler::syntax::parser::Parser;

pub mod compiler;

fn main() {
    let Ok(code) = fs::read_to_string("test.txt") else {
        println!("encountering fatal error when reading file");
        process::exit(1);
    };

    let mut parser = Parser::new(code.as_str());
    match parser.parse_program() {
        Ok(program) => println!("{program:#?}"),
        Err(e) => println!("{e:?}"),
    }
}
