#![allow(dead_code)]

use std::fs;
use std::path::Path;
use std::process::Command;
use std::rc::Rc;

use katastrophe::CompileResult;
use katastrophe::assemble;
use katastrophe::compiler::context::Context;
use katastrophe::compiler::err::CompileError;
use katastrophe::compiler::semantics::main_function_checker::main_function_check;
use katastrophe::ir_generate;
use katastrophe::ir_translate;
use katastrophe::lvalue_check;
use katastrophe::syntax_analyze;
use katastrophe::type_infer;
use katastrophe::util::file::gen_tmp_exe_path;

const PATH_TO_TEST_FILES: &str = "static/test_files";

pub fn test_with(test_name: &str) {
    let path_to_test = format!("{PATH_TO_TEST_FILES}/{test_name}");

    let files = fs::read_dir(path_to_test)
        .unwrap()
        .map(Result::unwrap)
        .map(|entry| {
            let os_filename = entry.file_name();
            let filename = os_filename.to_string_lossy();
            (filename.into_owned(), entry.path())
        })
        .map(Rc::new)
        .collect::<Rc<_>>();
    let (_, output) = files
        .iter()
        .map(Rc::as_ref)
        .find(|(filename, _)| filename == "output")
        .unwrap();
    let output = fs::read_to_string(output).unwrap();

    // TODO: support multi program files compilation
    let program = files
        .iter()
        .map(Rc::as_ref)
        .find(|(filename, _)| {
            Path::new(filename)
                .extension()
                .is_some_and(|extension| extension.eq_ignore_ascii_case("katas"))
        })
        .map(|(_, file)| file)
        .map(|file| fs::read_to_string(file).unwrap())
        .unwrap();

    test_output(&program, &output);
}

fn test_output(source_code: &str, expected: &str) {
    execute(|| {
        let output = compile_and_run(source_code)?;
        assert_eq!(output, expected);

        Ok(())
    });
}

pub fn assert_compile_fails(source_code: &str, is_expected: impl FnOnce(&CompileError) -> bool) {
    match compile_to_ir(source_code) {
        Ok(_) => panic!("expected compilation to fail."),
        Err(error) => {
            assert!(is_expected(&error), "unexpected compile error: {error}");
        }
    }
}

pub fn compile_to_ir(source_code: &str) -> CompileResult<String> {
    let mut context = Context::new();
    let main_document_id = syntax_analyze(&mut context, source_code)?;
    let mut ids = context.document_map.keys().copied().collect::<Vec<_>>();
    ids.sort_unstable();
    let ids = Rc::<[u32]>::from(ids);
    type_infer(&mut context, &ids)?;
    lvalue_check(&context, &ids)?;
    main_function_check(&context, main_document_id)?;
    ir_translate(&mut context, &ids)?;
    ir_generate(&context, &ids, main_document_id)
}

pub fn compile_to_executable(source_code: &str) -> CompileResult<String> {
    let ir_code = compile_to_ir(source_code)?;
    let path = gen_tmp_exe_path();
    assemble(ir_code, &path)?;
    Ok(path)
}

pub fn compile_and_run(source_code: &str) -> CompileResult<String> {
    let path = compile_to_executable(source_code)?;
    let output = Command::new(path).output().expect("Test code compile failed.");
    assert!(output.status.success(), "compiled program exited unsuccessfully.");
    let output = String::from_utf8(output.stdout).unwrap();
    Ok(output)
}

fn execute<F: FnOnce() -> CompileResult<()>>(f: F) {
    if let Err(e) = f() {
        panic!("unexpected compile error: {e}");
    }
}
