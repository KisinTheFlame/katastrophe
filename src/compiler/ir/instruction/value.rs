use std::fmt::Display;
use std::fmt::{self};
use std::rc::Rc;

use crate::util::common::Array;

use super::IrId;
use super::ir_type::IrType;
use super::memory::Memory;

#[derive(Clone)]
pub enum Value {
    Void,
    Register(IrId),
    ImmediateI32(i32),
    ImmediateI8(i8),
    ImmediateBool(bool),
    ImmediateStruct(Array<(Rc<Value>, Rc<IrType>)>),
    Pointer(Rc<Memory>),
    Parameter(IrId),
    Function(IrId),
    Label(IrId),
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Void => {
                panic!("void")
            }
            Value::Register(id) | Value::Parameter(id) => {
                write!(f, "%{id}")
            }
            Value::ImmediateI32(value) => write!(f, "{value}"),
            Value::ImmediateI8(value) => write!(f, "{value}"),
            Value::ImmediateBool(value) => {
                let value = i32::from(*value);
                write!(f, "{value}")
            }
            Value::ImmediateStruct(fields) => {
                let fields = fields
                    .iter()
                    .map(Rc::as_ref)
                    .map(|(field, ir_type)| format!("{ir_type} {field}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{{ {fields} }}")
            }
            Value::Function(id) => write!(f, "@{id}"),
            Value::Pointer(memory) => write!(f, "{memory}"),
            Value::Label(id) => write!(f, "{id}"),
        }
    }
}
