use std::fmt::{self, Display};

use crate::compiler::syntax::ast::ty::Type;

#[derive(Clone)]
pub enum IrType {
    Void,
    I32,
    Bool,
    Function {
        return_type: Box<IrType>,
        parameter_types: Vec<IrType>,
    },
}

impl From<Type> for IrType {
    fn from(value: Type) -> Self {
        Self::from(&value)
    }
}

impl From<&Type> for IrType {
    fn from(value: &Type) -> Self {
        match value {
            Type::Never => IrType::Void,
            Type::I32 => IrType::I32,
            Type::Bool => IrType::Bool,
            Type::Function {
                return_type,
                parameter_types,
            } => IrType::Function {
                return_type: Box::new(IrType::from(return_type.as_ref())),
                parameter_types: parameter_types.iter().map(IrType::from).collect(),
            },
            Type::Unknown => unimplemented!(),
        }
    }
}

impl Display for IrType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            IrType::Void => "void",
            IrType::I32 => "i32",
            IrType::Bool => "i1",
            IrType::Function {
                return_type: _,
                parameter_types: _,
            } => panic!("should never print function type"),
        };
        write!(f, "{s}")
    }
}
