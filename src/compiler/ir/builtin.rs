use super::{
    err::IrError,
    scope::{Scope, Tag},
};

pub fn init_builtin_scope(scope: &mut Scope, code_builder: &mut String) {
    scope.enter(Tag::Builtin);
    code_builder.push_str(
        "
declare i32 @exit(i32)
declare i32 @getchar()
declare i32 @putchar(i32)
",
    );
}

pub fn deinit_builtin_scope(scope: &mut Scope) -> Result<(), IrError> {
    scope.leave(Tag::Builtin)?;
    Ok(())
}
