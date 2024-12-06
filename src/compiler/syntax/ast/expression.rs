use std::fmt::Display;
use std::fmt::{self};
use std::rc::Rc;

use crate::util::common::Array;
use crate::util::pretty_format::PrettyFormat;
use crate::util::pretty_format::indent;

use crate::compiler::syntax::ast::ty::Type;

use super::crumb::Identifier;
use super::operator::Binary;
use super::operator::Unary;

#[derive(Clone)]
pub enum Expression {
    Identifier(Rc<Identifier>),

    IntLiteral(i32),
    CharLiteral(char),
    FloatLiteral(f64),
    BoolLiteral(bool),

    Unary(Unary, Rc<Type>, Rc<Expression>),
    Binary(Binary, Rc<Type>, Rc<Expression>, Rc<Expression>),

    Call(Rc<Identifier>, Array<Expression>),

    Cast(Rc<Expression>, Rc<Type>, Rc<Type>),
}

impl PrettyFormat for Expression {
    fn pretty_format(&self, f: &mut fmt::Formatter, indentation_num: usize) -> fmt::Result {
        let indentation = indent(indentation_num);
        match self {
            Expression::Identifier(identifier) => {
                writeln!(f, "{indentation}{identifier}")?;
            }
            Expression::IntLiteral(literal) => {
                writeln!(f, "{indentation}{literal}")?;
            }
            Expression::CharLiteral(literal) => {
                writeln!(f, "{indentation}'{literal}'")?;
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
                arguments
                    .iter()
                    .try_for_each(|arg| arg.pretty_format(f, indentation_num + 1))?;
            }
            Expression::Cast(expression, from_type, to_type) => {
                writeln!(f, "{indentation}As from {from_type} to {to_type}")?;
                expression.pretty_format(f, indentation_num + 1)?;
            }
        }
        Ok(())
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pretty_format(f, 0)
    }
}
