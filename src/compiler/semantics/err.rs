use std::rc::Rc;

use crate::compiler::syntax::ast::crumb::Identifier;
use crate::compiler::syntax::ast::operator::Binary;
use crate::compiler::syntax::ast::operator::Unary;
use crate::compiler::syntax::ast::ty::Type;
use crate::util::common::Array;
use crate::util::reportable_error::Reportable;

pub enum SemanticError {
    UndeclaredIdentifier(Rc<Identifier>),
    IllegalLValue,
    AssigningImmutableVariable(Rc<Identifier>),
}

impl Reportable for SemanticError {
    fn report(&self) -> ! {
        match self {
            SemanticError::UndeclaredIdentifier(identifier) => {
                panic!("undeclared identifier: {identifier}.")
            }
            SemanticError::IllegalLValue => panic!("illegal lvalue."),
            SemanticError::AssigningImmutableVariable(identifier) => {
                panic!("assigning immutable variable: {identifier}.")
            }
        }
    }
}

pub enum TypeError {
    ShouldBeFunctionType,

    UndefinedUnaryExpression(Unary, Rc<Type>),
    UndefinedBinaryExpression(Binary, Rc<Type>, Rc<Type>),
    ProcessInGlobal,

    ReturnTypeMismatch {
        expected: Rc<Type>,
        returned: Rc<Type>,
    },
    ConditionNeedBool,
    AssignTypeMismatch {
        lvalue_type: Rc<Type>,
        expression_type: Rc<Type>,
    },
    CallArgumentTypesMismatch {
        function_id: Rc<Identifier>,
        parameter_types: Array<Rc<Type>>,
        argument_types: Array<Rc<Type>>,
    },
    IllegalCast {
        from_type: Rc<Type>,
        to_type: Rc<Type>,
    },

    UndeclaredMainFunction,
    IllegalMainFunctionType,
}

impl Reportable for TypeError {
    fn report(&self) -> ! {
        match self {
            TypeError::ShouldBeFunctionType => {
                panic!("should be a function type.")
            }
            TypeError::UndefinedUnaryExpression(operator, ty) => {
                panic!("undefined unary expression: {operator} {ty}")
            }
            TypeError::UndefinedBinaryExpression(operator, t1, t2) => {
                panic!("undefined binary expression: {t1} {operator} {t2}")
            }
            TypeError::ProcessInGlobal => {
                panic!("process statements found in global.")
            }
            TypeError::ReturnTypeMismatch { expected, returned } => {
                panic!("return type mismatch. expected: {expected}, returned: {returned}.")
            }
            TypeError::ConditionNeedBool => {
                panic!("condition is supposed to be bool type.")
            }
            TypeError::AssignTypeMismatch {
                lvalue_type: declared_type,
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
                    .map(Rc::as_ref)
                    .map(Type::to_string)
                    .collect::<Rc<_>>()
                    .join(", ");
                let argument_types = argument_types
                    .iter()
                    .map(Rc::as_ref)
                    .map(Type::to_string)
                    .collect::<Rc<_>>()
                    .join(", ");
                panic!(
                    "arguments of call statement mismatch with parameters.
                function: {function_id},
                parameter types: {parameter_types},
                argument types: {argument_types}"
                )
            }
            TypeError::IllegalCast { from_type, to_type } => {
                panic!("illegal cast from {from_type} to {to_type}")
            }
            TypeError::UndeclaredMainFunction => {
                panic!("main function not declared in input document.")
            }
            TypeError::IllegalMainFunctionType => {
                panic!("the type of main function must be () -> i32.")
            }
        }
    }
}
