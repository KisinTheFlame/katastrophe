#![allow(dead_code)]

use std::fs;
use std::path::Path;
use std::path::PathBuf;
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
use katastrophe::syntax_analyze_path;
use katastrophe::type_infer;
use katastrophe::util::file::gen_tmp_exe_path;

const PATH_TO_TEST_FILES: &str = "static/test_files";

/// 单文件 fixture：fixture 目录里恰好一个 `.katas` 文件 + 一个 `output` 文件。
/// 走字符串编译入口，不开 `project_root`。
pub fn test_with(test_name: &str) {
    let path_to_test = format!("{PATH_TO_TEST_FILES}/{test_name}");
    let entries = fs::read_dir(&path_to_test)
        .unwrap_or_else(|_| panic!("missing fixture directory: {path_to_test}"))
        .map(Result::unwrap)
        .map(|entry| (entry.file_name().to_string_lossy().into_owned(), entry.path()))
        .collect::<Vec<_>>();
    let (_, output_path) = entries
        .iter()
        .find(|(name, _)| name == "output")
        .expect("fixture missing 'output' file");
    let expected = fs::read_to_string(output_path).unwrap();
    let (_, program_path) = entries
        .iter()
        .find(|(name, _)| {
            Path::new(name)
                .extension()
                .is_some_and(|ext| ext.eq_ignore_ascii_case("katas"))
        })
        .expect("fixture missing .katas source file");
    let program = fs::read_to_string(program_path).unwrap();
    test_output(&program, &expected);
}

/// 多文件 fixture：fixture 目录是项目根，含一个明确指定的入口文件 + `output`。
/// 走基于路径的编译入口（`syntax_analyze_path`），`project_root` 来自 fixture 目录。
pub fn test_project_with(test_name: &str, entry_filename: &str) {
    let path_to_test = PathBuf::from(format!("{PATH_TO_TEST_FILES}/{test_name}"));
    let entry_path = path_to_test.join(entry_filename);
    let output_path = path_to_test.join("output");
    let expected = fs::read_to_string(&output_path).unwrap_or_else(|_| {
        panic!("fixture missing 'output' file: {}", output_path.display());
    });
    execute(|| {
        let actual = compile_path_and_run(&entry_path)?;
        assert_eq!(actual, expected);
        Ok(())
    });
}

/// 多文件 fixture 的编译失败断言。`entry_filename` 在 fixture 目录下，`project_root` 也在那里。
pub fn assert_project_compile_fails(
    test_name: &str,
    entry_filename: &str,
    is_expected: impl FnOnce(&CompileError) -> bool,
) {
    let path_to_test = PathBuf::from(format!("{PATH_TO_TEST_FILES}/{test_name}"));
    let entry_path = path_to_test.join(entry_filename);
    match compile_path_to_ir(&entry_path) {
        Ok(_) => panic!("expected compilation to fail for fixture: {test_name}"),
        Err(error) => {
            assert!(is_expected(&error), "unexpected compile error: {error}");
        }
    }
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
    finish_pipeline(context, main_document_id)
}

pub fn compile_path_to_ir(entry_path: &Path) -> CompileResult<String> {
    let mut context = Context::new();
    let main_document_id = syntax_analyze_path(&mut context, entry_path)?;
    finish_pipeline(context, main_document_id)
}

fn finish_pipeline(mut context: Context, main_document_id: u32) -> CompileResult<String> {
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
    write_and_link(ir_code)
}

pub fn compile_path_to_executable(entry_path: &Path) -> CompileResult<String> {
    let ir_code = compile_path_to_ir(entry_path)?;
    write_and_link(ir_code)
}

fn write_and_link(ir_code: String) -> CompileResult<String> {
    let path = gen_tmp_exe_path()?;
    assemble(ir_code, &path)?;
    Ok(path)
}

pub fn compile_and_run(source_code: &str) -> CompileResult<String> {
    let path = compile_to_executable(source_code)?;
    Ok(run_executable(&path))
}

pub fn compile_path_and_run(entry_path: &Path) -> CompileResult<String> {
    let path = compile_path_to_executable(entry_path)?;
    Ok(run_executable(&path))
}

fn run_executable(path: &str) -> String {
    let output = Command::new(path).output().expect("Test code compile failed.");
    assert!(output.status.success(), "compiled program exited unsuccessfully.");
    String::from_utf8(output.stdout).unwrap()
}

fn execute<F: FnOnce() -> CompileResult<()>>(f: F) {
    if let Err(e) = f() {
        panic!("unexpected compile error: {e}");
    }
}
