use std::fmt;

pub trait PrettyFormat {
    /// # Errors
    fn pretty_format(&self, f: &mut fmt::Formatter, indentation_num: usize) -> fmt::Result;
}

#[must_use]
pub fn indent(n: usize) -> String {
    "    ".repeat(n)
}
