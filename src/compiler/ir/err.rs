use crate::{compiler::err::InnerCompilerError, util::reportable_error::ReportableError};

pub enum IrError {
    BuiltinFileNotExist,
    BuiltinFunctionFileNotExist(String),
}

impl InnerCompilerError for IrError {}

impl ReportableError for IrError {
    fn report(&self) -> ! {
        match &self {
            IrError::BuiltinFileNotExist => {
                panic!("builtin file not exist.");
            }
            IrError::BuiltinFunctionFileNotExist(identifier) => {
                panic!("builtin function {identifier} file not exist")
            }
        }
    }
}
