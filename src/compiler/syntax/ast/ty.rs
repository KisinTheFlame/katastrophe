use std::{
    fmt::{self, Display},
    hash::Hash,
    rc::Rc,
};

use crate::{compiler::{
    err::CompileError,
    syntax::err::{ParseError, ParseErrorKind},
}, util::common::Array};

#[derive(Clone)]
pub enum Type {
    Unknown,
    Never,
    I32,
    Bool,
    Function {
        return_type: Rc<Type>,
        parameter_types: Array<Rc<Type>>,
    },
}

impl Eq for Type {}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Unknown, Type::Unknown)
            | (Type::Never, Type::Never)
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
            Type::Never => {
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
                let parameter_types = parameter_types
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({parameter_types}) -> {return_type}")?;
            }
        };
        Ok(())
    }
}

impl TryFrom<String> for Type {
    type Error = CompileError;

    fn try_from(value: String) -> Result<Type, CompileError> {
        match value.as_str() {
            "void" => Ok(Type::Never),
            "i32" => Ok(Type::I32),
            "bool" => Ok(Type::Bool),
            _ => Err(ParseError {
                kind: ParseErrorKind::UnknownType(value),
            }
            .into()),
        }
    }
}
