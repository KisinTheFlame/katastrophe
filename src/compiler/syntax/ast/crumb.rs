use std::fmt::Display;
use std::fmt::{self};
use std::rc::Rc;

use crate::util::common::Array;
use crate::util::pretty_format::PrettyFormat;
use crate::util::pretty_format::indent;

use super::expression::Expression;
use super::ty::Type;

pub type Identifier = String;

#[derive(Clone)]
pub struct Parameter(pub Rc<Identifier>);

impl Display for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Parameter(identifier) = self;
        write!(f, "{identifier}")
    }
}

pub struct FunctionPrototype {
    pub identifier: Rc<Identifier>,
    pub parameters: Array<Parameter>,
    pub function_type: Rc<Type>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Mutability {
    Mutable,
    Immutable,
}

impl Display for Mutability {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Mutability::Mutable => write!(f, "mutable"),
            Mutability::Immutable => write!(f, "immutable"),
        }
    }
}

pub struct Variable(pub Rc<Identifier>, pub Rc<Type>, pub Mutability);

pub struct Field(pub Rc<Identifier>, pub Rc<Type>);

impl PrettyFormat for Field {
    fn pretty_format(&self, f: &mut fmt::Formatter, indentation_num: usize) -> fmt::Result {
        let Field(identifier, ty) = self;
        let indent = indent(indentation_num);
        writeln!(f, "{indent}{identifier} as {ty}")
    }
}

pub struct FieldInit(pub Rc<Identifier>, pub Rc<Expression>);

impl PrettyFormat for FieldInit {
    fn pretty_format(&self, f: &mut fmt::Formatter, indentation_num: usize) -> fmt::Result {
        let FieldInit(identifier, expression) = self;
        let indent = indent(indentation_num);
        writeln!(f, "{indent}{identifier} = {expression}")
    }
}
