use crate::{
    compiler::{err::InnerCompilerError, syntax::ast::Type},
    util::reportable_error::ReportableError,
};

pub enum SemanticError {
    UndeclaredIdentifier(String),
}

impl InnerCompilerError for SemanticError {}

impl ReportableError for SemanticError {
    fn report(&self) -> ! {
        panic!()
    }
}

pub enum TypeError {
    ShouldBeFunctionType,

    NoSuchOperator,
    ProcessInGlobal,

    ReturnTypeMismatch {
        expected: Type,
        returned: Type,
    },
    IfConditionNeedBool,
    LetAssignTypeMismatch {
        declared_type: Type,
        expression_type: Type,
    },
    CallArgumentTypesMismatch {
        function_id: String,
        parameter_types: Vec<Type>,
        argument_types: Vec<Type>,
    },
}

impl InnerCompilerError for TypeError {}

impl ReportableError for TypeError {
    fn report(&self) -> ! {
        match self {
            TypeError::ShouldBeFunctionType => {
                panic!("should be a function type.")
            }
            TypeError::NoSuchOperator => {
                panic!("no such operator.")
            }
            TypeError::ProcessInGlobal => {
                panic!("process statements found in global.")
            }
            TypeError::ReturnTypeMismatch { expected, returned } => {
                panic!("return type mismatch. expected: {expected}, returned: {returned}.")
            }
            TypeError::IfConditionNeedBool => {
                panic!("condition of if is supposed to be bool type.")
            }
            TypeError::LetAssignTypeMismatch {
                declared_type,
                expression_type,
            } => {
                panic!("assignment type mismatch. declared: {declared_type}, expression type: {expression_type}.")
            }
            TypeError::CallArgumentTypesMismatch {
                function_id,
                parameter_types,
                argument_types,
            } => {
                let parameter_types = parameter_types
                    .iter()
                    .map(Type::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                let argument_types = argument_types
                    .iter()
                    .map(Type::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                panic!(
                    "arguments of call statement mismatch with parameters.
                function: {function_id},
                parameter types: {parameter_types},
                argument types: {argument_types}"
                )
            }
        }
    }
}
