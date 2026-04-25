use std::fmt::Display;
use std::fmt::{self};
use std::rc::Rc;

use crate::compiler::bit_width::BitWidth;
use crate::compiler::context::StructId;
use crate::compiler::syntax::ast::crumb::Field;
use crate::compiler::syntax::ast::crumb::Identifier;
use crate::compiler::syntax::ast::ty::Type;
use crate::sys_error;
use crate::util::common::Array;

pub struct IrField(pub Rc<Identifier>, pub Rc<IrType>);

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
        fields: Array<IrField>,
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
                fields: fields
                    .iter()
                    .map(Rc::as_ref)
                    .map(|Field(field_name, field_type)| {
                        let field_type = IrType::from(field_type.as_ref());
                        IrField(field_name.clone(), field_type.into())
                    })
                    .map(Rc::from)
                    .collect(),
            },
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
            } => sys_error!("function type should never be formatted as a value type"),
            IrType::Struct { id, name, .. } => write!(f, "%s{id}.{name}"),
        }
    }
}

impl PartialEq for IrType {
    fn eq(&self, other: &Self) -> bool {
        use IrType::Bool;
        use IrType::Function;
        use IrType::Int;
        use IrType::Struct;
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
            (Struct { id: id1, .. }, Struct { id: id2, .. }) => id1 == id2,
            (_, _) => false,
        }
    }
}

impl Eq for IrType {}

/// # Panics
pub fn find_field(object_type: &Rc<IrType>, field: &Rc<Identifier>) -> (usize, Rc<IrType>) {
    let IrType::Struct { fields, .. } = object_type.as_ref() else {
        sys_error!("should be a struct type.");
    };
    let (index, IrField(_, field_type)) = fields
        .iter()
        .map(Rc::as_ref)
        .enumerate()
        .find(|(_, IrField(field_name, _))| field_name == field)
        .unwrap();
    (index, field_type.clone())
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use super::IrType;

    fn struct_type(id: u32) -> IrType {
        IrType::Struct {
            id,
            name: Rc::new(String::from("Person")),
            fields: [].into(),
        }
    }

    #[test]
    fn struct_type_equals_same_id() {
        assert!(struct_type(1) == struct_type(1));
    }

    #[test]
    fn struct_type_not_equals_different_id() {
        assert!(struct_type(1) != struct_type(2));
    }
}
