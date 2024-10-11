use std::rc::Rc;

use crate::util::{either::Either, reportable_error::Reportable};

#[macro_export]
macro_rules! sys_error {
    ($($arg:tt)*) => {
        Err(CompileError::from(format!($($arg)*)))
    };
}

pub struct CompileError(Either<Rc<dyn Reportable>, String>);

impl<T: Reportable + 'static> From<T> for CompileError {
    fn from(value: T) -> Self {
        CompileError(Either::Left(Rc::new(value)))
    }
}

impl From<String> for CompileError {
    fn from(value: String) -> Self {
        CompileError(Either::Right(value))
    }
}

impl CompileError {
    /// # Panics
    pub fn report(&self) -> ! {
        let CompileError(reason) = self;
        match reason {
            Either::Left(reason) => reason.report(),
            Either::Right(reason) => panic!("internal error: {reason}"),
        }
    }
}
