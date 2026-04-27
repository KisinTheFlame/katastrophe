use std::fs;
use std::path::Path;
use std::process::Command;
use std::rc::Rc;

use compiler::context::Context;
use compiler::err::CompileError;
use compiler::err::IceUnwrap;
use compiler::ir::builtin::generate_entry;
use compiler::ir::builtin::generate_libc_function;
use compiler::ir::instruction::IrReference;
use compiler::ir::translator::Translator;
use compiler::semantics::lvalue_checker::LValueChecker;
use compiler::semantics::type_inferrer::TypeInferrer;
use compiler::syntax::ast::package::DocumentPath;
use indoc::formatdoc;
use util::common::Arr;

use compiler::syntax::parser::Parser;
use util::file::gen_tmp_ir_path;

pub mod compiler;
pub mod constants;
pub mod util;

pub type CompileResult<T> = Result<T, CompileError>;

/// 用一段源码字符串作为入口编译，主要供测试与 `--ast` / `--ir` 之类无文件路径的场景使用。
/// `Context.project_root` 不会被设置；任何非 std 的 `using` 都会失败为 `UnknownPackage`。
///
/// # Errors
pub fn syntax_analyze(context: &mut Context, code: &str) -> CompileResult<u32> {
    let document_path = DocumentPath([Rc::new(String::from("__entry__"))].into()).into();
    let mut parser = Parser::new(document_path, code)?;
    let main_document_id = parser.parse_document(context)?;
    Ok(main_document_id)
}

/// 用一个 `.katas` 文件路径作为入口编译。会 `canonicalize` 入口、把其父目录写入
/// `Context.project_root`，并以入口文件名（去 `.katas` 后缀）作为入口的 `DocumentPath` 唯一节点。
/// 这是支持非 std 多文件项目的标准入口。
///
/// # Errors
pub fn syntax_analyze_path(context: &mut Context, entry_path: &Path) -> CompileResult<u32> {
    let canonical = fs::canonicalize(entry_path).map_err(|error| CompileError::EntryResolveFailed {
        path: entry_path.to_string_lossy().into_owned(),
        error: error.to_string(),
    })?;
    let project_root = canonical
        .parent()
        .ok_or_else(|| CompileError::EntryResolveFailed {
            path: canonical.to_string_lossy().into_owned(),
            error: String::from("entry path has no parent directory"),
        })?
        .to_path_buf();
    let entry_stem = canonical
        .file_stem()
        .ok_or_else(|| CompileError::EntryResolveFailed {
            path: canonical.to_string_lossy().into_owned(),
            error: String::from("entry path has no file stem"),
        })?
        .to_string_lossy()
        .into_owned();
    let code = fs::read_to_string(&canonical).map_err(|error| CompileError::FileReadFailed {
        path: canonical.to_string_lossy().into_owned(),
        error: error.to_string(),
    })?;

    context.project_root = project_root;
    let document_path = DocumentPath([Rc::new(entry_stem)].into()).into();
    let mut parser = Parser::new(document_path, &code)?;
    let main_document_id = parser.parse_document(context)?;
    Ok(main_document_id)
}

/// # Errors
pub fn type_infer(context: &mut Context, ids: &Arr<u32>) -> CompileResult<()> {
    let mut type_inferrer = TypeInferrer::new();
    ids.iter()
        .try_for_each(|id| type_inferrer.resolve_global_let_types(context, *id))?;
    ids.iter().try_for_each(|id| type_inferrer.infer(context, *id))?;
    Ok(())
}

/// # Errors
pub fn lvalue_check(context: &Context, ids: &Arr<u32>) -> CompileResult<()> {
    let mut lvalue_checker = LValueChecker::new();
    ids.iter()
        .try_for_each(|id| lvalue_checker.check_document(context, *id))?;
    Ok(())
}

/// 把上下文里所有文档的 AST 按文档 ID 升序渲染成可读文本。
///
/// # Panics
#[must_use]
pub fn dump_ast(context: &Context) -> String {
    let mut ids = context.document_map.keys().copied().collect::<Vec<_>>();
    ids.sort_unstable();
    ids.iter()
        .map(|id| {
            let document_path = context.path_map.get(id).or_ice("missing document path during ast dump");
            let ast = context.document_map.get(id).or_ice("missing document during ast dump");
            formatdoc! {"
                ----- {document_path} -----
                {ast}
            "}
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// # Errors
/// # Panics
pub fn ir_translate(context: &mut Context, ids: &Arr<u32>) -> CompileResult<()> {
    let mut id_translators = ids
        .iter()
        .map(|id| {
            let translator = Translator::new(
                context
                    .path_map
                    .get(id)
                    .or_ice("missing document path during ir_translate")
                    .clone(),
            );
            (*id, translator)
        })
        .collect::<Vec<_>>();
    id_translators
        .iter_mut()
        .try_for_each(|(id, translator)| translator.pre_scan_global(context, *id))?;
    id_translators
        .iter_mut()
        .try_for_each(|(id, translator)| translator.translate(context, *id))?;
    Ok(())
}

/// # Errors
/// # Panics
pub fn ir_generate(context: &Context, ids: &Arr<u32>, main_document_id: u32) -> CompileResult<String> {
    let ir = ids
        .iter()
        .map(|id| {
            let document_path = context
                .path_map
                .get(id)
                .or_ice("missing document path during ir_generate");
            let ir = context
                .instruction
                .get(id)
                .or_ice("missing instruction for document during ir_generate");
            formatdoc! {"
                ; ----- {document_path} -----
                {ir}
            "}
        })
        .collect::<Vec<_>>()
        .join("\n");
    let builtin_ir = generate_libc_function();
    let IrReference::Binding((main_value, _)) = context
        .ir_model_map
        .get(&main_document_id)
        .or_ice("missing ir_model_map for main document")
        .get(&String::from("main"))
        .or_ice("missing main reference in ir_model_map")
        .as_ref()
    else {
        sys_error!("failed to get main value");
    };
    let entry_ir = generate_entry(main_value.clone());
    let ir_code = formatdoc! {"
        {builtin_ir}
        {ir}
        {entry_ir}
    "};
    Ok(ir_code)
}

/// # Errors
pub fn assemble(ir_code: String, output_path: &str) -> CompileResult<()> {
    let temp_ir_path = gen_tmp_ir_path()?;
    fs::write(&temp_ir_path, ir_code).map_err(|error| CompileError::FileWriteFailed {
        path: temp_ir_path.clone(),
        error: error.to_string(),
    })?;

    let link_result = Command::new("clang")
        .arg(&temp_ir_path)
        .arg("-o")
        .arg(output_path)
        .output()
        .map_err(|error| CompileError::ExternalCommandFailed {
            command: "clang",
            error: error.to_string(),
        })?;
    if !link_result.status.success() {
        let error = String::from_utf8_lossy(&link_result.stderr).into_owned();
        return Err(CompileError::LinkFailed(error));
    }
    Ok(())
}
