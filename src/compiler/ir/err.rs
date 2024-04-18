use crate::compiler::syntax::ast::Type;

use super::scope::Tag;

#[derive(Debug)]
pub struct IrError {
    pub kind: IrErrorKind,
}

#[derive(Debug)]
pub enum IrErrorKind {
    NullScope,
    ScopeMismatch { expected: Tag, encountered: Tag },
    DuplicateIdentifierInSameScope,

    UndefinedMain,
    UndeclaredIdentifier(String),

    MissingDefinition,
    MissingEmpty,
    MissingBlock,
    MissingExpression,
    MissingReturn,
    MissingLet,
    UnknownType(Type),
    MismatchedType,

    BuiltinFileNotExist,
}
