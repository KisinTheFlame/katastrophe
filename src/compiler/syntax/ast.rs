use core::fmt;
use std::fmt::Display;

use crate::util::pretty_format::{indent, PrettyFormat};

use super::err::{ParseError, ParseErrorKind};

pub enum Type {
    Void,
    I32,
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let type_string = match self {
            Type::Void => "void",
            Type::I32 => "i32",
        };
        write!(f, "{type_string}")
    }
}

impl TryFrom<String> for Type {
    type Error = ParseError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        match value.as_str() {
            "void" => Ok(Type::Void),
            "i32" => Ok(Type::I32),
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

    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),

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
            Expression::Unary(operator, expression) => {
                writeln!(f, "{indentation}{operator}")?;
                expression.pretty_format(f, indentation_num + 1)?;
            }
            Expression::Binary(operator, left, right) => {
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

pub struct Parameter {
    pub identifier: String,
}

impl Display for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let identifier = &self.identifier;
        write!(f, "{identifier}")
    }
}

pub struct FunctionPrototype {
    pub identifier: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
}

pub enum Statement {
    Empty,
    Block(Vec<Statement>),
    Return(Expression),
    Expression(Expression),
    If {
        condition: Expression,
        body: Box<Statement>,
        else_body: Option<Box<Statement>>,
    },
    Let(String, Expression),
    Define {
        prototype: FunctionPrototype,
        body: Box<Statement>,
    },
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
                expression.pretty_format(f, indentation_num + 1)?;
            }
            Statement::Expression(_) => todo!(),
            Statement::If {
                condition,
                body,
                else_body,
            } => {
                writeln!(f, "{indentation}If")?;
                condition.pretty_format(f, indentation_num + 1)?;
                writeln!(f, "{indentation}Then")?;
                body.pretty_format(f, indentation_num + 1)?;
                if let Some(else_body) = else_body {
                    writeln!(f, "{indentation}Else")?;
                    else_body.pretty_format(f, indentation_num + 1)?;
                }
            }
            Statement::Let(identifier, expression) => {
                writeln!(f, "{indentation}Let {identifier}")?;
                expression.pretty_format(f, indentation_num + 1)?;
            }
            Statement::Define {
                prototype:
                    FunctionPrototype {
                        identifier,
                        parameters,
                        return_type,
                    },
                body,
            } => {
                let parameters = parameters
                    .iter()
                    .map(ToString::to_string)
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
