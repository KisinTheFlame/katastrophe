use crate::{
    compiler::{err::InnerCompilerError, syntax::ast::Type},
    util::reportable_error::ReportableError,
};

pub enum IrError {
    UndefinedMain,
    UndeclaredIdentifier(String),

    UnknownType(Type),
    MismatchedType,

    BuiltinFileNotExist,
}

impl InnerCompilerError for IrError {}

impl ReportableError for IrError {
    fn report(&self) -> ! {
        match &self {
            IrError::UndefinedMain => {
                panic!("main function not found in global scope.");
            }
            IrError::UndeclaredIdentifier(identifier) => {
                panic!("undeclared identifier {identifier} used.");
            }
            IrError::UnknownType(unknown_type) => {
                panic!("unknown type {unknown_type}.");
            }
            IrError::MismatchedType => {
                panic!("mismatched type.");
            }
            IrError::BuiltinFileNotExist => {
                panic!("builtin file not exist.");
            }
        }
    }
}
