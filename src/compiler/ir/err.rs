use crate::{compiler::syntax::ast::Type, util::reportable_error::ReportableError};

use super::scope::Tag;

pub enum IrErrorKind {
    NullScope,
    ScopeMismatch { expected: Tag, encountered: Tag },
    DuplicateIdentifierInSameScope(String),

    UndefinedMain,
    UndeclaredIdentifier(String),

    MissingDefinition,
    MissingBlock,
    MissingExpression,
    MissingReturn,
    MissingLet,
    UnknownType(Type),
    MismatchedType,

    BuiltinFileNotExist,
}

pub struct IrError {
    pub kind: IrErrorKind,
}

impl ReportableError for IrError {
    fn report(&self) -> ! {
        match &self.kind {
            IrErrorKind::NullScope => {
                panic!("null scope");
            }
            IrErrorKind::ScopeMismatch {
                expected,
                encountered,
            } => {
                panic!("scope mismatch. expected {expected}, encountered {encountered}.");
            }
            IrErrorKind::DuplicateIdentifierInSameScope(identifier) => {
                panic!("encountered duplicate identifier {identifier} in same scope.");
            }
            IrErrorKind::UndefinedMain => {
                panic!("main function not found in global scope.");
            }
            IrErrorKind::UndeclaredIdentifier(identifier) => {
                panic!("undeclared identifier {identifier} used.");
            }
            IrErrorKind::MissingDefinition => {
                panic!("missing definition.");
            }
            IrErrorKind::MissingBlock => {
                panic!("missing block.");
            }
            IrErrorKind::MissingExpression => {
                panic!("missing expression.");
            }
            IrErrorKind::MissingReturn => {
                panic!("missing return.");
            }
            IrErrorKind::MissingLet => {
                panic!("missing let.")
            }
            IrErrorKind::UnknownType(unknown_type) => {
                panic!("unknown type {unknown_type}.");
            }
            IrErrorKind::MismatchedType => {
                panic!("mismatched type.");
            }
            IrErrorKind::BuiltinFileNotExist => {
                panic!("builtin file not exist.");
            }
        }
    }
}
