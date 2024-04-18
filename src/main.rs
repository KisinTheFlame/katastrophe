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
    let link_result = Command::new("clang")
        .arg("test.ll")
        .arg("-o")
        .arg("test")
        .output()
        .expect("failed to compile llvm ir.");
    if !link_result.status.success() {
        let error = String::from_utf8(link_result.stderr).expect("failed to read stderr");
        panic!("{error}");
    }
    let status = Command::new("./test")
        .output()
        .expect("failed to execute.")
        .status;
    println!("{status}");
}
