use std::fs;

use crate::compiler::{err::CompileError, scope::Tag};

use super::{err::IrError, instruction::IrScope};

pub fn init_builtin_scope(
    scope: &mut IrScope,
    code_builder: &mut String,
) -> Result<(), CompileError> {
    scope.enter(Tag::Builtin);
    let builtin = fs::read_to_string("static/builtin.ll")
        .or::<CompileError>(Err(IrError::BuiltinFileNotExist.into()))?;
    code_builder.push_str(&builtin);
    Ok(())
}

pub fn deinit_builtin_scope(scope: &mut IrScope) -> Result<(), CompileError> {
    scope.leave(Tag::Builtin)?;
    Ok(())
}
