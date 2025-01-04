use std::rc::Rc;

use crate::CompileResult;
use crate::compiler::bit_width::BitWidth;
use crate::compiler::context::Context;
use crate::compiler::context::DocumentId;
use crate::compiler::err::CompileError;
use crate::compiler::syntax::ast::reference::Reference;
use crate::compiler::syntax::ast::ty::Type;
use crate::sys_error;

/// # Errors
pub fn main_function_check(context: &Context, main_document_id: DocumentId) -> CompileResult<()> {
    let Some(reference_map) = context.reference_map.get(&main_document_id) else {
        sys_error!("reference map must exist");
    };
    let Some(Reference::Binding(main_type, _)) = reference_map.get(&String::from("main")).map(Rc::as_ref) else {
        return Err(CompileError::UndeclaredMainFunction);
    };
    let expected_main_function_type = Type::Function {
        return_type: Type::Int(BitWidth::Bit32).into(),
        parameter_types: [].into(),
    };
    if *main_type.as_ref() != expected_main_function_type {
        return Err(CompileError::IllegalMainFunctionType);
    }
    Ok(())
}
