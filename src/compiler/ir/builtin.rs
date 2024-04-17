use std::fs;

use super::{
    err::{IrError, IrErrorKind},
    scope::{Scope, Tag},
};

pub fn init_builtin_scope(scope: &mut Scope, code_builder: &mut String) -> Result<(), IrError> {
    scope.enter(Tag::Builtin);
    let builtin = fs::read_to_string("static/builtin.ll").or(Err(IrError {
        kind: IrErrorKind::BuiltinFileNotExist,
    }))?;
    code_builder.push_str(&builtin);
    Ok(())
}

pub fn deinit_builtin_scope(scope: &mut Scope) -> Result<(), IrError> {
    scope.leave(Tag::Builtin)?;
    Ok(())
}
