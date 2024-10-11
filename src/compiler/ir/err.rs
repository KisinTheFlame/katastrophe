use std::rc::Rc;

use crate::{compiler::syntax::ast::crumb::Identifier, util::reportable_error::Reportable};

pub enum IrError {
    BuiltinFileNotExist,
    BuiltinFunctionFileNotExist(Rc<Identifier>),
}

impl Reportable for IrError {
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
