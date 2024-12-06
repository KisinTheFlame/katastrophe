use std::fs;
use std::process::Command;
use std::rc::Rc;

use katastrophe::assemble;
use katastrophe::compiler::context::Context;
use katastrophe::compiler::err::CompileError;
use katastrophe::compiler::semantics::main_function_checker::main_function_check;
use katastrophe::ir_generate;
use katastrophe::ir_translate;
use katastrophe::mutability_check;
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
        .find(|(filename, _)| filename.ends_with(".katas"))
        .map(|(_, file)| file)
        .map(|file| fs::read_to_string(file).unwrap())
        .unwrap();

    test_output(&program, output);
}

fn test_output(source_code: &str, expected: String) {
    execute(|| {
        let path = compile(source_code)?;

        let output = Command::new(path).output().expect("Test code compile failed.");
        let output = String::from_utf8(output.stdout).unwrap();
        assert_eq!(output, expected);

        Ok(())
    });
}

fn compile(source_code: &str) -> Result<String, CompileError> {
    let mut context = Context::new();
    let main_document_id = syntax_analyze(&mut context, source_code)?;
    let mut ids = context.document_map.keys().copied().collect::<Vec<_>>();
    ids.sort_unstable();
    let ids = Rc::<[u32]>::from(ids);
    type_infer(&mut context, &ids)?;
    mutability_check(&context, &ids)?;
    main_function_check(&context, main_document_id)?;
    ir_translate(&mut context, &ids)?;
    let ir_code = ir_generate(&context, &ids, main_document_id)?;
    let path = gen_tmp_exe_path();
    assemble(ir_code, path.clone());
    Ok(path)
}

fn execute<F: FnOnce() -> Result<(), CompileError>>(f: F) {
    if let Err(e) = f() {
        e.report()
    }
}
