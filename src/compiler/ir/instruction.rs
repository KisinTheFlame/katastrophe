use std::fmt::Display;
use std::fmt::{self};
use std::rc::Rc;

use ir_type::IrField;
use value::Value;

use crate::compiler::context::StructId;
use crate::compiler::scope::Scope;
use crate::compiler::syntax::ast::crumb::Identifier;
use crate::util::common::Array;
use crate::util::pretty_format::PrettyFormat;
use crate::util::pretty_format::indent;

use self::ir_type::IrType;

pub mod ir_type;
pub mod memory;
pub mod value;

pub type IrId = String;

pub type IrModel = (Rc<Value>, Rc<IrType>);

pub type IrScope = Scope<IrReference>;

#[derive(Clone)]
pub enum IrReference {
    Binding(IrModel),
    StructDef(StructId, Rc<Identifier>, Array<IrField>),
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
    Struct {
        name: Rc<Identifier>,
        fields: Array<IrType>,
    },
    GetElementPtr {
        from: Rc<Value>,
        to: Rc<Value>,
        data_type: Rc<IrType>,
        index: usize,
    },
    ExtractValue {
        from: Rc<Value>,
        to: Rc<Value>,
        data_type: Rc<IrType>,
        index: usize,
    },
    ReturnVoid,
    Return {
        return_type: Rc<IrType>,
        value: Rc<Value>,
    },
    Batch(Array<Instruction>),
    Definition(IrFunctionPrototype, Array<Value>, Rc<Instruction>),
    BuiltinDefinition(String),
    Binary {
        operator: IrBinaryOpcode,
        data_type: Rc<IrType>,
        result: Rc<Value>,
        left: Rc<Value>,
        right: Rc<Value>,
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
        arguments: Array<Value>,
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
            Instruction::Struct { name, fields } => {
                let fields = fields
                    .iter()
                    .map(|field| format!("{field}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                writeln!(f, "{indentation}%{name} = type {{ {fields} }}")
            }
            Instruction::GetElementPtr {
                from,
                to,
                data_type,
                index,
            } => {
                writeln!(
                    f,
                    "{indentation}{to} = getelementptr {data_type}, ptr {from}, i32 {index}"
                )
            }
            Instruction::ExtractValue {
                from,
                to,
                data_type,
                index,
            } => {
                writeln!(f, "{indentation}{to} = extractvalue {data_type} {from}, {index}")
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
