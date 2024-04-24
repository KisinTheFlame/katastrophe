use crate::compiler::{
    context::{Context, DocumentId},
    err::CompileError,
    syntax::ast::ty::Type,
};

use super::err::TypeError;

pub fn main_function_check(
    context: &Context,
    main_document_id: DocumentId,
) -> Result<(), CompileError> {
    let type_map = context.type_map.get(&main_document_id).unwrap();
    let main_type = match type_map.get(&String::from("main")) {
        Some(main_type) => main_type,
        None => return Err(TypeError::UndeclaredMainFunction.into()),
    };
    let expected_main_function_type = Type::Function {
        return_type: Box::new(Type::I32),
        parameter_types: Vec::new(),
    };
    if main_type != &expected_main_function_type {
        return Err(TypeError::IllegalMainFunctionType.into());
    }
    Ok(())
}
