use std::rc::Rc;

use crate::compiler::err::CompileError;

use super::crumb::Identifier;
use super::expression::Expression;

pub enum LValue {
    Id(Rc<Identifier>),
}

impl LValue {
    #[must_use]
    pub fn root(&self) -> Rc<Identifier> {
        match self {
            LValue::Id(id) => id.clone(),
        }
    }
}

impl TryFrom<&Expression> for LValue {
    type Error = CompileError;

    fn try_from(expression: &Expression) -> Result<Self, Self::Error> {
        match expression {
            Expression::Identifier(id) => Ok(LValue::Id(id.clone())),
            _ => Err(CompileError::IllegalLValue),
        }
    }
}
