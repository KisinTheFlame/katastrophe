use crate::{
    compiler::{
        context::{Context, DocumentId},
        err::CompileError,
        syntax::ast::ty::Type,
    },
    sys_error,
};

use super::err::TypeError;

/// # Errors
pub fn main_function_check(
    context: &Context,
    main_document_id: DocumentId,
) -> Result<(), CompileError> {
    let Some(type_map) = context.type_map.get(&main_document_id) else {
        return sys_error!("type map must exist");
    };
    let Some(main_type) = type_map.get(&String::from("main")) else {
        return Err(TypeError::UndeclaredMainFunction.into());
    };
    let expected_main_function_type = Type::Function {
        return_type: Type::I32.into(),
        parameter_types: [].into(),
    };
    if *main_type.as_ref() != expected_main_function_type {
        return Err(TypeError::IllegalMainFunctionType.into());
    }
    Ok(())
}