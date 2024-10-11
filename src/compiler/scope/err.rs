use std::rc::Rc;

use crate::{compiler::syntax::ast::crumb::Identifier, util::reportable_error::Reportable};

use super::Tag;

pub enum ScopeError {
    NullScope,
    ScopeMismatch { expected: Tag, encountered: Tag },
    DuplicateIdentifierInSameScope(Rc<Identifier>),

    NotInFunction,
}

impl Reportable for ScopeError {
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
