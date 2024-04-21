use std::fmt::{self, Display};

use super::ty::Type;

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

pub struct Variable(pub String, pub Type, pub Mutability);
