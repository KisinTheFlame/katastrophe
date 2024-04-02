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

pub enum Value {
    Register(u32),
    Immediate(i32),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Register(id) => write!(f, "%{id}"),
            Value::Immediate(value) => write!(f, "{value}"),
        }
    }
}

pub struct Parameter {
    // data_type: DataType,
    // id: u32,
}

pub enum Instruction {
    NoOperation,
    Global {
        id: u32,
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
        id: u32,
        parameters: Vec<Parameter>,
        body: Box<Instruction>,
    },
    Bitcast {
        from: Value,
        to: Value,
    },
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::NoOperation => Ok(()),
            Instruction::Global {
                id,
                data_type,
                value,
            } => {
                write!(f, "@{id} = global {data_type} {value}")
            }
            Instruction::Return { data_type, value } => {
                write!(f, "ret {data_type} {value}")
            }
            Instruction::Batch(instructions) => {
                for instruction in instructions {
                    instruction.fmt(f)?;
                    writeln!(f)?;
                }
                Ok(())
            }
            Instruction::Function {
                return_type,
                id,
                parameters: _,
                body,
            } => {
                writeln!(f, "define {return_type} @{id}() {{")?;
                body.fmt(f)?;
                write!(f, "}}")?;
                Ok(())
            }
            Instruction::Bitcast { from, to } => write!(f, "{to} = bitcast i32 {from} to i32"),
        }
    }
}
