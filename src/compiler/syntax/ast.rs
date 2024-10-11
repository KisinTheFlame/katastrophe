use core::fmt;

use std::{fmt::Display, rc::Rc};

use crate::util::{common::Array, pretty_format::PrettyFormat};

use self::statement::Statement;

pub mod crumb;
pub mod expression;
pub mod operator;
pub mod package;
pub mod statement;
pub mod ty;

pub struct Document {
    pub statements: Array<Rc<Statement>>,
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
