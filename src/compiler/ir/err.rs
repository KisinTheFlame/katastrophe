use crate::{compiler::err::InnerCompilerError, util::reportable_error::ReportableError};

pub enum IrError {
    UndefinedMain,
    BuiltinFileNotExist,
}

impl InnerCompilerError for IrError {}

impl ReportableError for IrError {
    fn report(&self) -> ! {
        match &self {
            IrError::UndefinedMain => {
                panic!("main function not found in global scope.");
            }
            IrError::BuiltinFileNotExist => {
                panic!("builtin file not exist.");
            }
        }
    }
}
