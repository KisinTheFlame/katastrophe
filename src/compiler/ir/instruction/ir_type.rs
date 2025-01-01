use std::fmt::Display;
use std::fmt::{self};
use std::rc::Rc;

use crate::compiler::bit_width::BitWidth;
use crate::compiler::context::StructId;
use crate::compiler::syntax::ast::crumb::Field;
use crate::compiler::syntax::ast::crumb::Identifier;
use crate::compiler::syntax::ast::ty::Type;
use crate::util::common::Array;

#[derive(Clone)]
pub enum IrType {
    Void,
    Int(BitWidth),
    Bool,
    Function {
        return_type: Rc<IrType>,
        parameter_types: Array<IrType>,
    },
    Struct {
        id: StructId,
        name: Rc<Identifier>,
        field_types: Array<IrType>,
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
            Type::Int(bit_width) => IrType::Int(*bit_width),
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
            Type::Struct { id, name, fields } => IrType::Struct {
                id: *id,
                name: name.clone(),
                field_types: fields
                    .iter()
                    .map(Rc::as_ref)
                    .map(|Field(_, field_type)| field_type)
                    .map(Rc::as_ref)
                    .map(IrType::from)
                    .map(Rc::from)
                    .collect(),
            },
            Type::Unknown => unimplemented!(),
        }
    }
}

impl Display for IrType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrType::Void => write!(f, "void"),
            IrType::Int(bit_width) => write!(f, "i{bit_width}"),
            IrType::Bool => write!(f, "i1"),
            IrType::Function {
                return_type: _,
                parameter_types: _,
            } => panic!("should never print function type"),
            IrType::Struct {
                id,
                name,
                field_types: _,
            } => write!(f, "%s{id}.{name}"),
        }
    }
}

impl PartialEq for IrType {
    fn eq(&self, other: &Self) -> bool {
        use IrType::Bool;
        use IrType::Function;
        use IrType::Int;
        use IrType::Void;
        match (self, other) {
            (Void, Void) | (Bool, Bool) => true,
            (Int(w1), Int(w2)) => w1 == w2,
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
