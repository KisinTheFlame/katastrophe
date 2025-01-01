use std::rc::Rc;

use crate::compiler::err::CompileError;

use super::crumb::Identifier;
use super::expression::Expression;

pub enum LValue {
    Id(Rc<Identifier>),
    Access(Rc<LValue>, Rc<Identifier>),
}

impl LValue {
    #[must_use]
    pub fn root(&self) -> Rc<Identifier> {
        match self {
            LValue::Id(id) => id.clone(),
            LValue::Access(object, _) => object.root(),
        }
    }
}

impl TryFrom<Rc<Expression>> for LValue {
    type Error = CompileError;

    fn try_from(value: Rc<Expression>) -> Result<Self, Self::Error> {
        LValue::try_from(value.as_ref())
    }
}

impl TryFrom<&Expression> for LValue {
    type Error = CompileError;

    fn try_from(expression: &Expression) -> Result<Self, Self::Error> {
        let lvalue = match expression {
            Expression::Identifier(id) => LValue::Id(id.clone()),
            Expression::Access(object, _, field) => {
                LValue::Access(LValue::try_from(object.as_ref())?.into(), field.clone())
            }
            _ => return Err(CompileError::IllegalLValue),
        };
        Ok(lvalue)
    }
}
