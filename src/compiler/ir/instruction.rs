use std::fmt;

pub enum DataType {
    Void,
    I32,
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            DataType::Void => "void",
            DataType::I32 => "i32",
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
    Function(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Register(id) | Value::StackPointer(id) => write!(f, "%{id}"),
            Value::Immediate(value) => write!(f, "{value}"),
            Value::GlobalPointer(id) | Value::Function(id) => write!(f, "@{id}"),
        }
    }
}

pub struct Parameter {
    // data_type: DataType,
    // id: u32,
}

pub enum BinaryOperation {
    Add,
    Subtract,
    Multiply,
    DivideSigned,
}

impl fmt::Display for BinaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperation::Add => write!(f, "add"),
            BinaryOperation::Subtract => write!(f, "sub"),
            BinaryOperation::Multiply => write!(f, "mul"),
            BinaryOperation::DivideSigned => write!(f, "sdiv"),
        }
    }
}

pub enum Instruction {
    NoOperation,
    Global {
        lvalue: Value,
        data_type: DataType,
        value: Value,
    },
    Return {
        data_type: DataType,
        value: Value,
    },
    Batch(Vec<Instruction>),
    Function {
        return_type: DataType,
        id: Value,
        parameters: Vec<Parameter>,
        body: Box<Instruction>,
    },
    Bitcast {
        from: Value,
        to: Value,
    },
    Binary {
        operator: BinaryOperation,
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
}

impl Instruction {
    fn gracefully_format(&self, f: &mut fmt::Formatter, indentation_num: usize) -> fmt::Result {
        let indentation = "    ".repeat(indentation_num);
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
                    instruction.gracefully_format(f, indentation_num)?;
                }
                Ok(())
            }
            Instruction::Function {
                return_type,
                id,
                parameters: _,
                body,
            } => {
                writeln!(f, "{indentation}define {return_type} {id}() {{")?;
                body.gracefully_format(f, indentation_num + 1)?;
                writeln!(f, "{indentation}}}")?;
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
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.gracefully_format(f, 0)
    }
}
