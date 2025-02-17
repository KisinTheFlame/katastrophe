use core::fmt;

use std::fmt::Display;

use crate::util::common::Array;
use crate::util::pretty_format::PrettyFormat;

use self::statement::Statement;

pub mod crumb;
pub mod expression;
pub mod lvalue;
pub mod operator;
pub mod package;
pub mod reference;
pub mod statement;
pub mod ty;

pub struct Document {
    pub statements: Array<Statement>,
}

impl PrettyFormat for Document {
    fn pretty_format(&self, f: &mut fmt::Formatter, indentation_num: usize) -> fmt::Result {
        self.statements
            .iter()
            .try_for_each(|statement| statement.pretty_format(f, indentation_num))
    }
}

impl Display for Document {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pretty_format(f, 0)
    }
}
