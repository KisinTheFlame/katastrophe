use crate::util::{either::Either, reportable_error::ReportableError};

pub trait InnerCompilerError: ReportableError {}

#[macro_export]
macro_rules! system_error {
    ($($arg:tt)*) => {
        CompileError::from(format!($($arg)*))
    };
}

pub struct CompileError(Either<Box<dyn InnerCompilerError>, String>);

impl<T: InnerCompilerError + 'static> From<T> for CompileError {
    fn from(value: T) -> Self {
        CompileError(Either::Left(Box::new(value)))
    }
}

impl From<String> for CompileError {
    fn from(value: String) -> Self {
        CompileError(Either::Right(value))
    }
}

impl ReportableError for CompileError {
    fn report(&self) -> ! {
        let CompileError(reason) = self;
        match reason {
            Either::Left(reason) => reason.report(),
            Either::Right(reason) => panic!("{reason}"),
        }
    }
}
