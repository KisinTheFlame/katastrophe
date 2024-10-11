use std::{
    fmt::{self, Display},
    rc::Rc,
};

use crate::{compiler::syntax::ast::ty::Type, util::common::Array};

#[derive(Clone)]
pub enum IrType {
    Void,
    I32,
    Bool,
    Function {
        return_type: Rc<IrType>,
        parameter_types: Array<Rc<IrType>>,
    },
}

impl From<Rc<Type>> for IrType {
    fn from(value: Rc<Type>) -> Self {
        value.as_ref().into()
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
                return_type: Rc::new(IrType::from(return_type.as_ref())),
                parameter_types: parameter_types
                    .iter()
                    .map(Rc::as_ref)
                    .map(IrType::from)
                    .map(Rc::new)
                    .collect(),
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

impl PartialEq for IrType {
    fn eq(&self, other: &Self) -> bool {
        use IrType::{Bool, Function, Void, I32};
        match (self, other) {
            (Void, Void) | (I32, I32) | (Bool, Bool) => true,
            (
                Function {
                    return_type: r1,
                    parameter_types: p1,
                },
                Function {
                    return_type: r2,
                    parameter_types: p2,
                },
            ) => r1 == r2 && p1 == p2,
            (_, _) => false,
        }
    }
}
