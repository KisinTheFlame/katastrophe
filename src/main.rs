use std::fs;
use std::process::Command;

use compiler::ir::translator::Translator;
use compiler::syntax::parser::Parser;

pub mod compiler;

fn main() {
    let Ok(code) = fs::read_to_string("test.katas") else {
        panic!("encountering fatal error when reading file");
    };

    let mut parser = Parser::new(code.as_str());
    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(e) => {
            panic!("{e:?}");
        }
    };
    let mut translator = Translator::new();
    match translator.translate(program) {
        Ok(program) => fs::write("test.ll", program).expect("failed to write llvm ir."),
        Err(e) => {
            panic!("{e:?}")
        }
    };
    Command::new("clang")
        .arg("test.ll")
        .arg("-o")
        .arg("test")
        .output()
        .expect("failed to compile llvm ir.");
    let status = Command::new("./test")
        .output()
        .expect("failed to execute.")
        .status;
    println!("{status}");
}
