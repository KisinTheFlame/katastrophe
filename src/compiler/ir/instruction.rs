use std::fmt::{self, Display};

use crate::{
    compiler::syntax::ast::Type,
    util::pretty_format::{indent, PrettyFormat},
};

use super::err::IrError;

#[derive(Clone)]
pub enum IrType {
    Void,
    I32,
    Function {
        return_type: Box<IrType>,
        parameter_types: Vec<IrType>,
    },
}

impl TryFrom<Type> for IrType {
    type Error = IrError;

    fn try_from(value: Type) -> Result<Self, Self::Error> {
        let result = match value {
            Type::Void => IrType::Void,
            Type::I32 => IrType::I32,
        };
        Ok(result)
    }
}

impl Display for IrType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            IrType::Void => "void",
            IrType::I32 => "i32",
            IrType::Function {
                return_type: _,
                parameter_types: _,
            } => return Err(fmt::Error),
        };
        write!(f, "{s}")
    }
}

#[derive(Clone)]
pub enum Value {
    Register(String),
    Immediate(i32),
    StackPointer(String),
    GlobalPointer(String),
    Parameter(String),
    Function(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Register(id) | Value::StackPointer(id) | Value::Parameter(id) => {
                write!(f, "%{id}")
            }
            Value::Immediate(value) => write!(f, "{value}"),
            Value::GlobalPointer(id) | Value::Function(id) => write!(f, "@{id}"),
        }
    }
}

pub struct IrFunctionPrototype {
    pub function_type: IrType,
    pub id: Value,
}

pub enum IrBinaryOpcode {
    Add,
    Subtract,
    Multiply,
    DivideSigned,
}

impl Display for IrBinaryOpcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrBinaryOpcode::Add => write!(f, "add"),
            IrBinaryOpcode::Subtract => write!(f, "sub"),
            IrBinaryOpcode::Multiply => write!(f, "mul"),
            IrBinaryOpcode::DivideSigned => write!(f, "sdiv"),
        }
    }
}

pub enum Instruction {
    NoOperation,
    Global {
        lvalue: Value,
        data_type: IrType,
        value: Value,
    },
    Return {
        data_type: IrType,
        value: Value,
    },
    Batch(Vec<Instruction>),
    Definition(IrFunctionPrototype, Vec<Value>, Box<Instruction>),
    Bitcast {
        from: Value,
        to: Value,
    },
    Binary {
        operator: IrBinaryOpcode,
        result: Value,
        left: Value,
        right: Value,
    },
    Allocate(Value),
    Load {
        from: Value,
        to: Value,
    },
    Store {
        from: Value,
        to: Value,
    },
    Call {
        receiver: Option<Value>,
        function: IrFunctionPrototype,
        arguments: Vec<Value>,
    },
}

impl PrettyFormat for Instruction {
    fn pretty_format(&self, f: &mut fmt::Formatter, indentation_num: usize) -> fmt::Result {
        let indentation = indent(indentation_num);
        match self {
            Instruction::NoOperation => Ok(()),
            Instruction::Global {
                lvalue,
                data_type,
                value,
            } => {
                writeln!(f, "{indentation}{lvalue} = global {data_type} {value}")
            }
            Instruction::Return { data_type, value } => {
                writeln!(f, "{indentation}ret {data_type} {value}")
            }
            Instruction::Batch(instructions) => {
                for instruction in instructions {
                    instruction.pretty_format(f, indentation_num)?;
                }
                Ok(())
            }
            Instruction::Definition(
                IrFunctionPrototype { function_type, id },
                parameters,
                body,
            ) => {
                let IrType::Function {
                    return_type,
                    parameter_types,
                } = function_type
                else {
                    return Err(fmt::Error);
                };

                let parameters = parameter_types
                    .iter()
                    .zip(parameters)
                    .map(|(param_type, param)| format!("{param_type} {param}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                writeln!(f, "{indentation}define {return_type} {id}({parameters}) {{")?;
                body.pretty_format(f, indentation_num + 1)?;
                writeln!(f, "{indentation}}}")?;
                writeln!(f)?;
                Ok(())
            }
            Instruction::Bitcast { from, to } => {
                writeln!(f, "{indentation}{to} = bitcast i32 {from} to i32")
            }
            Instruction::Binary {
                operator,
                result,
                left,
                right,
            } => {
                writeln!(f, "{indentation}{result} = {operator} i32 {left}, {right}")
            }
            Instruction::Allocate(pointer) => {
                writeln!(f, "{indentation}{pointer} = alloca i32")
            }
            Instruction::Load { from, to } => {
                writeln!(f, "{indentation}{to} = load i32, ptr {from}")
            }
            Instruction::Store { from, to } => {
                writeln!(f, "{indentation}store i32 {from}, ptr {to}")
            }
            Instruction::Call {
                receiver,
                function,
                arguments,
            } => {
                let receiver = receiver
                    .as_ref()
                    .map_or(String::new(), |receiver| format!("{receiver} = "));
                let function_type = &function.function_type;
                let IrType::Function {
                    return_type,
                    parameter_types: parameter_type,
                } = function_type
                else {
                    return Err(fmt::Error);
                };
                let id = &function.id;
                let arguments = arguments
                    .iter()
                    .zip(parameter_type.iter())
                    .map(|(argument, parameter_type)| format!("{parameter_type} {argument}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                writeln!(
                    f,
                    "{indentation}{receiver}call {return_type} {id}({arguments})"
                )
            }
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pretty_format(f, 0)
    }
}
