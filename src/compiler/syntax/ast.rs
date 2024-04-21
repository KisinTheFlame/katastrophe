use core::fmt;
use std::{fmt::Display, hash::Hash};

use crate::util::pretty_format::{indent, PrettyFormat};

use super::err::{ParseError, ParseErrorKind};

#[derive(Clone)]
pub enum Type {
    Unknown,
    Void,
    I32,
    Bool,
    Function {
        return_type: Box<Type>,
        parameter_types: Vec<Type>,
    },
}

impl Type {
    fn format_parameter_types(parameter_types: &[Type]) -> String {
        parameter_types
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ")
    }
}

impl Eq for Type {}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Unknown, Type::Unknown)
            | (Type::Void, Type::Void)
            | (Type::I32, Type::I32)
            | (Type::Bool, Type::Bool) => true,
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
            Type::Void => {
                write!(f, "void")?;
            }
            Type::I32 => {
                write!(f, "i32")?;
            }
            Type::Bool => {
                write!(f, "bool")?;
            }
            Type::Function {
                return_type,
                parameter_types,
            } => {
                let parameter_types = Type::format_parameter_types(parameter_types);
                write!(f, "({parameter_types}) -> {return_type}")?;
            }
        };
        Ok(())
    }
}

impl TryFrom<String> for Type {
    type Error = ParseError;

    fn try_from(value: String) -> Result<Type, ParseError> {
        match value.as_str() {
            "void" => Ok(Type::Void),
            "i32" => Ok(Type::I32),
            "bool" => Ok(Type::Bool),
            _ => Err(ParseError {
                kind: ParseErrorKind::UnknownType(value),
            }),
        }
    }
}

pub trait Operator {
    /// (unary) - ! ~ : 14 right
    /// * / : 13 left
    /// + - : 12 left
    /// < > <= >= : 10 left
    /// == != : 9 left
    /// & : 8 left
    /// ^ : 7 left
    /// | : 6 left
    /// && : 5 left
    /// || : 4 left
    /// (assign) = : 2 right
    fn precedence(&self) -> u8;
    fn is_left_associative(&self) -> bool;
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
    LogicalNot,
    BitNot,
    Negative,
}

impl Operator for UnaryOperator {
    fn precedence(&self) -> u8 {
        match self {
            UnaryOperator::LogicalNot | UnaryOperator::BitNot | UnaryOperator::Negative => 14u8,
        }
    }

    fn is_left_associative(&self) -> bool {
        match self {
            UnaryOperator::LogicalNot | UnaryOperator::BitNot | UnaryOperator::Negative => false,
        }
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            UnaryOperator::LogicalNot => "LogicalNot",
            UnaryOperator::BitNot => "BitNot",
            UnaryOperator::Negative => "Negative",
        };
        write!(f, "{s}")
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,

    LogicalAnd,
    LogicalOr,

    BitAnd,
    BitOr,

    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

impl Operator for BinaryOperator {
    fn precedence(&self) -> u8 {
        match self {
            BinaryOperator::Multiply | BinaryOperator::Divide => 13u8,
            BinaryOperator::Add | BinaryOperator::Subtract => 12u8,
            BinaryOperator::LessThan
            | BinaryOperator::LessThanEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterThanEqual => 10u8,
            BinaryOperator::Equal | BinaryOperator::NotEqual => 9u8,
            BinaryOperator::BitAnd => 8u8,
            BinaryOperator::BitOr => 6u8,
            BinaryOperator::LogicalAnd => 5u8,
            BinaryOperator::LogicalOr => 4u8,
        }
    }

    fn is_left_associative(&self) -> bool {
        match self {
            BinaryOperator::Add
            | BinaryOperator::Subtract
            | BinaryOperator::Multiply
            | BinaryOperator::Divide
            | BinaryOperator::LogicalAnd
            | BinaryOperator::LogicalOr
            | BinaryOperator::BitAnd
            | BinaryOperator::BitOr
            | BinaryOperator::Equal
            | BinaryOperator::NotEqual
            | BinaryOperator::LessThan
            | BinaryOperator::LessThanEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterThanEqual => true,
        }
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            BinaryOperator::Add => "Add",
            BinaryOperator::Subtract => "Subtract",
            BinaryOperator::Multiply => "Multiply",
            BinaryOperator::Divide => "Divide",
            BinaryOperator::LogicalAnd => "LogicalAnd",
            BinaryOperator::LogicalOr => "LogicalOr",
            BinaryOperator::BitAnd => "BitAnd",
            BinaryOperator::BitOr => "BitOr",
            BinaryOperator::Equal => "Equal",
            BinaryOperator::NotEqual => "NotEqual",
            BinaryOperator::LessThan => "LessThan",
            BinaryOperator::LessThanEqual => "LessThanEqual",
            BinaryOperator::GreaterThan => "GreaterThan",
            BinaryOperator::GreaterThanEqual => "GreaterThanEqual",
        };
        write!(f, "{s}")
    }
}

pub enum Expression {
    Identifier(String),

    IntLiteral(i32),
    FloatLiteral(f64),
    BoolLiteral(bool),

    Unary(UnaryOperator, Type, Box<Expression>),
    Binary(BinaryOperator, Type, Box<Expression>, Box<Expression>),

    Call(String, Vec<Expression>),
}

impl PrettyFormat for Expression {
    fn pretty_format(
        &self,
        f: &mut std::fmt::Formatter,
        indentation_num: usize,
    ) -> std::fmt::Result {
        let indentation = indent(indentation_num);
        match self {
            Expression::Identifier(identifier) => {
                writeln!(f, "{indentation}{identifier}")?;
            }
            Expression::IntLiteral(literal) => {
                writeln!(f, "{indentation}{literal}")?;
            }
            Expression::FloatLiteral(literal) => {
                writeln!(f, "{indentation}{literal}")?;
            }
            Expression::BoolLiteral(literal) => {
                writeln!(f, "{indentation}{literal}")?;
            }
            Expression::Unary(operator, _, expression) => {
                writeln!(f, "{indentation}{operator}")?;
                expression.pretty_format(f, indentation_num + 1)?;
            }
            Expression::Binary(operator, _, left, right) => {
                writeln!(f, "{indentation}{operator}")?;
                left.pretty_format(f, indentation_num + 1)?;
                right.pretty_format(f, indentation_num + 1)?;
            }
            Expression::Call(callee, arguments) => {
                writeln!(f, "{indentation}Call {callee}")?;
                for arg in arguments {
                    arg.pretty_format(f, indentation_num + 1)?;
                }
            }
        }
        Ok(())
    }
}

pub struct Parameter(pub String);

impl Display for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Parameter(identifier) = self;
        write!(f, "{identifier}")
    }
}

pub struct FunctionPrototype {
    pub identifier: String,
    pub parameters: Vec<Parameter>,
    pub function_type: Type,
}

pub struct Variable(pub String, pub Type);

pub struct IfDetail {
    pub condition: Expression,
    pub true_body: Box<Statement>,
    pub false_body: Option<Box<Statement>>,
}

pub struct LetDetail(pub Variable, pub Expression);

pub struct DefineDetail {
    pub prototype: FunctionPrototype,
    pub body: Box<Statement>,
}

pub enum Statement {
    Empty,
    Block(Vec<Statement>),
    Return(Option<Expression>),
    Expression(Expression),
    If(IfDetail),
    Let(LetDetail),
    Define(DefineDetail),
}

impl PrettyFormat for Statement {
    fn pretty_format(
        &self,
        f: &mut std::fmt::Formatter,
        indentation_num: usize,
    ) -> std::fmt::Result {
        let indentation = indent(indentation_num);
        match self {
            Statement::Empty => {}
            Statement::Block(statements) => {
                for statement in statements {
                    statement.pretty_format(f, indentation_num)?;
                }
            }
            Statement::Return(expression) => {
                writeln!(f, "{indentation}Return")?;
                expression
                    .as_ref()
                    .map(|e| e.pretty_format(f, indentation_num + 1));
            }
            Statement::Expression(expression) => {
                expression.pretty_format(f, indentation_num)?;
            }
            Statement::If(IfDetail {
                condition,
                true_body: body,
                false_body: else_body,
            }) => {
                writeln!(f, "{indentation}If")?;
                condition.pretty_format(f, indentation_num + 1)?;
                writeln!(f, "{indentation}Then")?;
                body.pretty_format(f, indentation_num + 1)?;
                if let Some(else_body) = else_body {
                    writeln!(f, "{indentation}Else")?;
                    else_body.pretty_format(f, indentation_num + 1)?;
                }
            }
            Statement::Let(LetDetail(variable, expression)) => {
                let Variable(id, var_type) = variable;
                writeln!(f, "{indentation}Let {id} as {var_type}")?;
                expression.pretty_format(f, indentation_num + 1)?;
            }
            Statement::Define(DefineDetail {
                prototype:
                    FunctionPrototype {
                        identifier,
                        parameters,
                        function_type,
                    },
                body,
            }) => {
                let Type::Function {
                    return_type,
                    parameter_types,
                } = function_type
                else {
                    panic!("must be a function type");
                };
                let parameters = parameters
                    .iter()
                    .zip(parameter_types)
                    .map(|(parameter, param_type)| format!("{parameter} as {param_type}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                writeln!(
                    f,
                    "{indentation}Define {identifier}({parameters}) -> {return_type}"
                )?;
                body.pretty_format(f, indentation_num + 1)?;
            }
        };
        Ok(())
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl PrettyFormat for Program {
    fn pretty_format(
        &self,
        f: &mut std::fmt::Formatter,
        indentation_num: usize,
    ) -> std::fmt::Result {
        for statement in &self.statements {
            statement.pretty_format(f, indentation_num)?;
        }
        Ok(())
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pretty_format(f, 0)
    }
}
