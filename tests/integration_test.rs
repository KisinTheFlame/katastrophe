use std::path::PathBuf;

use helper::assert_project_compile_fails;
use helper::compile_path_to_ir;
use helper::test_project_with;
use helper::test_with;
use katastrophe::compiler::err::CompileError;

mod helper;

#[test]
fn test_minimal() {
    test_with("minimal");
}

#[test]
fn test_hello_world() {
    test_with("hello_world");
}

#[test]
fn test_fibonacci() {
    test_with("fibonacci");
}

#[test]
fn test_i8() {
    test_with("i8");
}

#[test]
fn test_struct() {
    test_with("struct");
}

#[test]
fn test_shadowing() {
    test_with("shadowing");
}

// --- 多文件 / 项目根 import 测试（T1-T8 + T_builtin）---

#[test]
fn test_multi_file() {
    test_project_with("multi_file", "main.katas");
}

#[test]
fn test_missing_package_file() {
    assert_project_compile_fails(
        "missing_package",
        "main.katas",
        |error| matches!(error, CompileError::UnknownPackage { path } if path.contains("nonexistent")),
    );
}

#[test]
fn test_circular_import() {
    test_project_with("circular_import", "main.katas");
}

#[test]
fn test_sibling_imports_entry() {
    // 侧文件 b.katas 通过入口 file_stem (`main`) 引入入口的 helper 函数。
    // F1 修复的关键：入口注册为 DocumentPath(["main"])，sibling 的 `using main::helper` 命中 id_map，
    // 不会重复 parse 入口文件。
    test_project_with("self_import", "main.katas");
}

#[test]
fn test_case_mismatch() {
    // mac 上即使 utils.katas 存在（小写），using Utils::greet 也应被拒。
    assert_project_compile_fails(
        "case_mismatch",
        "main.katas",
        |error| matches!(error, CompileError::UnknownPackage { path } if path.contains("Utils")),
    );
}

#[test]
fn test_user_cannot_shadow_std() {
    // 项目根放了一份不合法的 std/io.katas，但 std 永远走嵌入版本——这个用户文件不被读。
    test_project_with("user_cannot_shadow_std", "main.katas");
}

#[test]
fn test_empty_file_module() {
    // 项目根含空白的 utils.katas，没人 import 它，编译应正常完成。
    test_project_with("empty_file_module", "main.katas");
}

#[test]
fn test_builtin_outside_std() {
    assert_project_compile_fails(
        "builtin_outside_std",
        "main.katas",
        |error| matches!(error, CompileError::BuiltinNotAllowedOutsideStd(name) if name.as_str() == "foo"),
    );
}

#[test]
fn test_entry_resolve_failed() {
    let bogus = PathBuf::from("/nonexistent/path/to/never/exist.katas");
    let result = compile_path_to_ir(&bogus);
    assert!(matches!(result, Err(CompileError::EntryResolveFailed { .. })));
}
