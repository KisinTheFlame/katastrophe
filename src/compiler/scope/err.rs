use crate::{compiler::err::InnerCompilerError, util::reportable_error::ReportableError};

use super::Tag;

pub enum ScopeError {
    NullScope,
    ScopeMismatch { expected: Tag, encountered: Tag },
    DuplicateIdentifierInSameScope(String),

    NotInFunction,
}

impl InnerCompilerError for ScopeError {}

impl ReportableError for ScopeError {
    fn report(&self) -> ! {
        match self {
            ScopeError::NullScope => {
                panic!("null scope.");
            }
            ScopeError::ScopeMismatch {
                expected,
                encountered,
            } => {
                panic!("scope mismatch. expected {expected}, encountered {encountered}.");
            }
            ScopeError::DuplicateIdentifierInSameScope(identifier) => {
                panic!("encountered duplicate identifier {identifier} in same scope.")
            }
            ScopeError::NotInFunction => {
                panic!("outside any function.")
            }
        }
    }
}
