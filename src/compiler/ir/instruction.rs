use std::fmt::Display;
use std::fmt::{self};
use std::rc::Rc;

use crate::compiler::scope::Scope;
use crate::util::common::Array;
use crate::util::pretty_format::PrettyFormat;
use crate::util::pretty_format::indent;

use self::ir_type::IrType;

pub mod ir_type;

pub type IrId = String;

pub type IrModel = (Rc<Value>, Rc<IrType>);

pub type IrScope = Scope<IrModel>;

#[derive(Clone)]
pub enum Value {
    Void,
    Register(IrId),
    ImmediateI32(i32),
    ImmediateI8(i8),
    ImmediateBool(bool),
    StackPointer(IrId),
    GlobalPointer(IrId),
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
            Value::Register(id) | Value::StackPointer(id) | Value::Parameter(id) => {
                write!(f, "%{id}")
            }
            Value::ImmediateI32(value) => write!(f, "{value}"),
            Value::ImmediateI8(value) => write!(f, "{value}"),
            Value::ImmediateBool(value) => {
                let value = i32::from(*value);
                write!(f, "{value}")
            }
            Value::GlobalPointer(id) | Value::Function(id) => write!(f, "@{id}"),
            Value::Label(id) => write!(f, "{id}"),
        }
    }
}

pub struct IrFunctionPrototype {
    pub function_type: Rc<IrType>,
    pub id: Rc<Value>,
}

pub enum IrBinaryOpcode {
    Add,
    Subtract,
    Multiply,
    DivideSigned,

    And,
    Or,
    Xor,

    LeftShift,
    ArithmeticRightShift,

    Compare(Comparator),
}

impl Display for IrBinaryOpcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrBinaryOpcode::Add => write!(f, "add"),
            IrBinaryOpcode::Subtract => write!(f, "sub"),
            IrBinaryOpcode::Multiply => write!(f, "mul"),
            IrBinaryOpcode::DivideSigned => write!(f, "sdiv"),
            IrBinaryOpcode::And => write!(f, "and"),
            IrBinaryOpcode::Or => write!(f, "or"),
            IrBinaryOpcode::Xor => write!(f, "xor"),
            IrBinaryOpcode::LeftShift => write!(f, "shl"),
            IrBinaryOpcode::ArithmeticRightShift => write!(f, "ashr"),
            IrBinaryOpcode::Compare(comparator) => write!(f, "icmp {comparator}"),
        }
    }
}

pub enum Comparator {
    Equal,
    NotEqual,
    SignedLessThan,
    SignedLessThanEqual,
    SignedGreaterThan,
    SignedGreaterThanEqual,
}

impl Display for Comparator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let comparator = match self {
            Comparator::Equal => "eq",
            Comparator::NotEqual => "ne",
            Comparator::SignedLessThan => "slt",
            Comparator::SignedLessThanEqual => "sle",
            Comparator::SignedGreaterThan => "sgt",
            Comparator::SignedGreaterThanEqual => "sge",
        };
        write!(f, "{comparator}")
    }
}

pub enum Instruction {
    NoOperation,
    Global {
        lvalue: Rc<Value>,
        data_type: Rc<IrType>,
        value: Rc<Value>,
    },
    Constant {
        lvalue: Rc<Value>,
        data_type: Rc<IrType>,
        value: Rc<Value>,
    },
    ReturnVoid,
    Return {
        return_type: Rc<IrType>,
        value: Rc<Value>,
    },
    Batch(Array<Rc<Instruction>>),
    Definition(IrFunctionPrototype, Array<Rc<Value>>, Rc<Instruction>),
    BuiltinDefinition(String),
    Binary {
        operator: IrBinaryOpcode,
        data_type: Rc<IrType>,
        result: Rc<Value>,
        left: Rc<Value>,
        right: Rc<Value>,
    },
    Copy {
        data_type: Rc<IrType>,
        from: Rc<Value>,
        to: Rc<Value>,
    },
    Bitcast {
        from: IrModel,
        to: IrModel,
    },
    Truncate {
        from: IrModel,
        to: IrModel,
    },
    SignExtension {
        from: IrModel,
        to: IrModel,
    },
    Allocate(IrModel),
    Load {
        data_type: Rc<IrType>,
        from: Rc<Value>,
        to: Rc<Value>,
    },
    Store {
        data_type: Rc<IrType>,
        from: Rc<Value>,
        to: Rc<Value>,
    },
    Call {
        receiver: Option<Rc<Value>>,
        function: IrFunctionPrototype,
        arguments: Array<Rc<Value>>,
    },
    Label(Rc<Value>),
    Jump(Rc<Value>),
    JumpIf {
        condition: Rc<Value>,
        when_true: Rc<Value>,
        when_false: Rc<Value>,
    },
    Unreachable,
}

impl Instruction {
    fn format_definition(&self, f: &mut fmt::Formatter, indentation_num: usize) -> fmt::Result {
        let Instruction::Definition(IrFunctionPrototype { function_type, id }, parameters, body) = self else {
            return Err(fmt::Error);
        };
        let IrType::Function {
            return_type,
            parameter_types,
        } = function_type.as_ref()
        else {
            return Err(fmt::Error);
        };

        let parameters = parameter_types
            .iter()
            .zip(parameters.iter())
            .map(|(param_type, param)| format!("{param_type} {param}"))
            .collect::<Vec<_>>()
            .join(", ");

        let indentation = indent(indentation_num);
        writeln!(f, "{indentation}define {return_type} {id}({parameters}) {{")?;
        body.pretty_format(f, indentation_num + 1)?;
        writeln!(f, "{indentation}}}")?;
        writeln!(f)?;
        Ok(())
    }

    fn format_call(&self, f: &mut fmt::Formatter, indentation_num: usize) -> fmt::Result {
        let Instruction::Call {
            receiver,
            function,
            arguments,
        } = &self
        else {
            return Err(fmt::Error);
        };
        let receiver = receiver
            .as_ref()
            .map_or(String::new(), |receiver| format!("{receiver} = "));
        let function_type = &function.function_type;
        let IrType::Function {
            return_type,
            parameter_types: parameter_type,
        } = function_type.as_ref()
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

        let indentation = indent(indentation_num);
        writeln!(f, "{indentation}{receiver}call {return_type} {id}({arguments})")
    }

    fn format_copy(&self, f: &mut fmt::Formatter, indentation_num: usize) -> fmt::Result {
        let Instruction::Copy { data_type, from, to } = &self else {
            return Err(fmt::Error);
        };
        let from_value = from.clone();
        let from_type = data_type.clone();
        let to_value = to.clone();
        let to_type = data_type.clone();
        Instruction::Bitcast {
            from: (from_value, from_type),
            to: (to_value, to_type),
        }
        .pretty_format(f, indentation_num)
    }
}

impl PrettyFormat for Instruction {
    #[allow(clippy::too_many_lines)]
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
            Instruction::Constant {
                lvalue,
                data_type,
                value,
            } => {
                writeln!(f, "{indentation}{lvalue} = constant {data_type} {value}")
            }
            Instruction::ReturnVoid => {
                writeln!(f, "{indentation}ret void")
            }
            Instruction::Return {
                return_type: data_type,
                value,
            } => {
                writeln!(f, "{indentation}ret {data_type} {value}")
            }
            Instruction::Batch(instructions) => {
                instructions
                    .iter()
                    .try_for_each(|instruction| instruction.pretty_format(f, indentation_num))?;
                Ok(())
            }
            Instruction::Definition(_, _, _) => self.format_definition(f, indentation_num),
            Instruction::BuiltinDefinition(ir_code) => writeln!(f, "{ir_code}"),
            Instruction::Binary {
                operator,
                data_type,
                result,
                left,
                right,
            } => {
                writeln!(f, "{indentation}{result} = {operator} {data_type} {left}, {right}")
            }
            Instruction::Copy {
                data_type: _,
                from: _,
                to: _,
            } => self.format_copy(f, indentation_num),
            Instruction::Bitcast {
                from: (from_value, from_type),
                to: (to_value, to_type),
            } => {
                writeln!(
                    f,
                    "{indentation}{to_value} = bitcast {from_type} {from_value} to {to_type}"
                )
            }
            Instruction::Truncate {
                from: (from_value, from_type),
                to: (to_value, to_type),
            } => {
                writeln!(
                    f,
                    "{indentation}{to_value} = trunc {from_type} {from_value} to {to_type}"
                )
            }
            Instruction::SignExtension {
                from: (from_value, from_type),
                to: (to_value, to_type),
            } => writeln!(
                f,
                "{indentation}{to_value} = sext {from_type} {from_value} to {to_type}"
            ),
            Instruction::Allocate((pointer, data_type)) => {
                writeln!(f, "{indentation}{pointer} = alloca {data_type}")
            }
            Instruction::Load { data_type, from, to } => {
                writeln!(f, "{indentation}{to} = load {data_type}, ptr {from}")
            }
            Instruction::Store { data_type, from, to } => {
                writeln!(f, "{indentation}store {data_type} {from}, ptr {to}")
            }
            Instruction::Call {
                receiver: _,
                function: _,
                arguments: _,
            } => self.format_call(f, indentation_num),
            Instruction::Label(label) => {
                writeln!(f, "{label}:")
            }
            Instruction::Jump(label) => {
                writeln!(f, "{indentation}br label %{label}")
            }
            Instruction::JumpIf {
                condition,
                when_true,
                when_false,
            } => {
                writeln!(
                    f,
                    "{indentation}br i1 {condition}, label %{when_true}, label %{when_false}"
                )
            }
            Instruction::Unreachable => {
                writeln!(f, "{indentation}unreachable")
            }
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pretty_format(f, 0)
    }
}
