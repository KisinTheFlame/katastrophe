use core::fmt;
use std::fmt::Display;

use crate::util::pretty_format::PrettyFormat;

use self::{statement::Statement, ty::Type};

pub mod crumb;
pub mod expression;
pub mod operator;
pub mod statement;
pub mod ty;

pub struct Program {
    pub statements: Vec<Statement>,
}

impl PrettyFormat for Program {
    fn pretty_format(
        &self,
        f: &mut std::fmt::Formatter,
        indentation_num: usize,
    ) -> std::fmt::Result {
        self.statements
            .iter()
            .try_for_each(|statement| statement.pretty_format(f, indentation_num))
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pretty_format(f, 0)
    }
}
