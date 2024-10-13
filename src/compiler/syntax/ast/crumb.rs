use std::fmt::Display;
use std::fmt::{self};
use std::rc::Rc;

use crate::util::common::Array;

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
    pub parameters: Array<Rc<Parameter>>,
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
