use crate::util::reportable_error::ReportableError;

pub trait InnerCompilerError: ReportableError {}

pub struct CompileError {
    kind: Box<dyn InnerCompilerError>,
}

impl<T: InnerCompilerError + 'static> From<T> for CompileError {
    fn from(value: T) -> Self {
        CompileError {
            kind: Box::new(value),
        }
    }
}

impl ReportableError for CompileError {
    fn report(&self) -> ! {
        self.kind.report()
    }
}
