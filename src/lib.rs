use std::fs;
use std::process::Command;
use std::rc::Rc;

use compiler::context::Context;
use compiler::err::CompileError;
use compiler::ir::builtin::{generate_entry, generate_libc_function};
use compiler::ir::translator::Translator;
use compiler::semantics::mutability_checker::MutabilityChecker;
use compiler::semantics::type_inferrer::TypeInferrer;
use compiler::syntax::ast::package::DocumentPath;
use indoc::formatdoc;
use util::common::Array;

use compiler::syntax::parser::Parser;
use util::file::gen_tmp_ir_path;

pub mod compiler;
pub mod constants;
pub mod util;

/// # Errors
pub fn syntax_analyze(context: &mut Context, code: &str) -> Result<u32, CompileError> {
    let document_path = DocumentPath([Rc::new(String::from("self"))].into()).into();
    let mut parser = Parser::new(document_path, code);
    let main_document_id = parser.parse_document(context)?;
    Ok(main_document_id)
}

/// # Errors
pub fn type_infer(context: &mut Context, ids: &Array<u32>) -> Result<(), CompileError> {
    let mut type_inferrer = TypeInferrer::new();
    ids.iter()
        .try_for_each(|id| type_inferrer.infer(context, *id))?;
    Ok(())
}

/// # Errors
pub fn mutability_check(context: &Context, ids: &Array<u32>) -> Result<(), CompileError> {
    let mut mutability_checker = MutabilityChecker::new();
    ids.iter()
        .try_for_each(|id| mutability_checker.check_document(context, *id))?;
    Ok(())
}

/// # Errors
/// # Panics
pub fn ir_translate(context: &mut Context, ids: &Array<u32>) -> Result<(), CompileError> {
    let mut id_translators = ids
        .iter()
        .map(|id| {
            let translator = Translator::new(context.path_map.get(id).unwrap().clone());
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
pub fn ir_generate(
    context: &Context,
    ids: &Array<u32>,
    main_document_id: u32,
) -> Result<String, CompileError> {
    let ir = ids
        .iter()
        .map(|id| {
            let document_path = context.path_map.get(id).unwrap();
            let ir = context.instruction.get(id).unwrap();
            formatdoc! {"
                ; ----- {document_path} -----
                {ir}
            "}
        })
        .collect::<Vec<_>>()
        .join("\n");
    let builtin_ir = generate_libc_function()?;
    let (main_value, _) = context
        .ir_model_map
        .get(&main_document_id)
        .unwrap()
        .get(&String::from("main"))
        .unwrap();
    let entry_ir = generate_entry(main_value.clone())?;
    let ir_code = formatdoc! {"
        {builtin_ir}
        {ir}
        {entry_ir}
    "};
    Ok(ir_code)
}

/// # Panics
pub fn assemble(ir_code: String, output_path: String) {
    let temp_ir_path = gen_tmp_ir_path();
    fs::write(&temp_ir_path, ir_code).expect("failed to open a temp ir file");

    let link_result = Command::new("clang")
        .arg("-mllvm")
        .arg("-opaque-pointers")
        .arg(&temp_ir_path)
        .arg("-o")
        .arg(output_path)
        .output()
        .expect("failed to compile ir.");
    if !link_result.status.success() {
        let error = String::from_utf8(link_result.stderr).expect("failed to read stderr");
        panic!("{error}");
    }
}
