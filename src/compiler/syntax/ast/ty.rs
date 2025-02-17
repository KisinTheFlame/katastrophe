use std::fmt::Display;
use std::fmt::{self};
use std::hash::Hash;
use std::rc::Rc;

use crate::compiler::bit_width::BitWidth;
use crate::compiler::context::StructId;
use crate::util::common::Array;

use super::crumb::Field;
use super::crumb::Identifier;

#[derive(Clone)]
pub enum Type {
    Unknown,
    Never,
    Int(BitWidth),
    Bool,
    Function {
        return_type: Rc<Type>,
        parameter_types: Array<Type>,
    },
    Struct {
        id: StructId,
        name: Rc<Identifier>,
        fields: Array<Field>,
    },
}

impl Eq for Type {}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Unknown, Type::Unknown) | (Type::Never, Type::Never) | (Type::Bool, Type::Bool) => true,
            (Type::Int(w1), Type::Int(w2)) => w1 == w2,
            (
                Type::Function {
                    return_type: r1,
                    parameter_types: p1,
                },
                Type::Function {
                    return_type: r2,
                    parameter_types: p2,
                },
            ) => r1 == r2 && p1 == p2,
            (_, _) => false,
        }
    }
}

impl Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.to_string().hash(state);
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unknown => {
                write!(f, "unknown")?;
            }
            Type::Never => {
                write!(f, "void")?;
            }
            Type::Int(bit_width) => {
                write!(f, "i{bit_width}")?;
            }
            Type::Bool => {
                write!(f, "bool")?;
            }
            Type::Function {
                return_type,
                parameter_types,
            } => {
                let parameter_types = parameter_types
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({parameter_types}) -> {return_type}")?;
            }
            Type::Struct { id, name, .. } => {
                write!(f, "struct {name}({id})")?;
            }
        };
        Ok(())
    }
}
