use std::rc::Rc;

use crate::util::common::Array;

use super::lexis::token::Keyword;
use super::lexis::token::Symbol;
use super::lexis::token::Token;
use super::scope::Tag;
use super::syntax::ast::crumb::Identifier;
use super::syntax::ast::operator::Binary;
use super::syntax::ast::operator::Unary;
use super::syntax::ast::ty::Type;

#[macro_export]
macro_rules! sys_error {
    ($($arg:tt)*) => {
        panic!($($arg)*)
    };
}

pub enum CompileError {
    // Lexis Errors
    IllegalIntegerLiteral(String),
    IllegalFloatLiteral(String),
    IllegalEscapeChar(char),

    UnexpectedCharacter(char),
    UnexpectedLexEOF,

    // Parse Errors
    MissingIdentifier,
    MissingKeyword(Keyword),
    MissingSymbol(Symbol),

    UnexpectedParseEOF,
    UnexpectedToken(Token),

    UnknownType(String),
    UnknownPackage,

    // Semantics Errors
    UndeclaredIdentifier(Rc<Identifier>),
    IllegalLValue,
    AssigningImmutableVariable(Rc<Identifier>),

    // Type Errors
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

    // Ir Errors
    BuiltinFileNotExist,
    BuiltinFunctionFileNotExist(Rc<Identifier>),

    // Scope Errors
    NullScope,
    ScopeMismatch {
        expected: Tag,
        encountered: Tag,
    },
    DuplicateIdentifierInSameScope(Rc<Identifier>),

    NotInFunction,
}

impl CompileError {
    /// # Panics
    #[allow(clippy::too_many_lines)]
    pub fn report(self) -> ! {
        match self {
            // Lex Errors
            CompileError::IllegalIntegerLiteral(ref s)
            | CompileError::IllegalFloatLiteral(ref s) => {
                panic!("encountering illegal literal: {s}")
            }
            CompileError::IllegalEscapeChar(c) => {
                panic!("illegal escape char: {c}")
            }
            CompileError::UnexpectedCharacter(c) => {
                panic!("encountering unexpected character: {c}")
            }
            CompileError::UnexpectedLexEOF => panic!("code should not end here"),

            // Parse Errors
            CompileError::MissingIdentifier => {
                panic!("failed to expect an identifier")
            }
            CompileError::MissingKeyword(keyword) => {
                panic!("missing expected keyword: {keyword:?}")
            }
            CompileError::MissingSymbol(symbol) => {
                panic!("missing expected symbol: {symbol:?}")
            }
            CompileError::UnexpectedParseEOF => {
                panic!("encountering unexpected EOF")
            }
            CompileError::UnexpectedToken(token) => {
                panic!("encountering unexpected token: {token:?}")
            }
            CompileError::UnknownType(unknown_type) => {
                panic!("encountering unknown type: {unknown_type}")
            }
            CompileError::UnknownPackage => {
                panic!("unknown package.")
            }

            // Semantics Errors
            CompileError::UndeclaredIdentifier(identifier) => {
                panic!("undeclared identifier: {identifier}.")
            }
            CompileError::IllegalLValue => {
                panic!("illegal lvalue.")
            }
            CompileError::AssigningImmutableVariable(identifier) => {
                panic!("assigning immutable variable: {identifier}.")
            }

            // Type Errors
            CompileError::ShouldBeFunctionType => {
                panic!("should be a function type.")
            }
            CompileError::UndefinedUnaryExpression(operator, ty) => {
                panic!("undefined unary expression: {operator} {ty}")
            }
            CompileError::UndefinedBinaryExpression(operator, t1, t2) => {
                panic!("undefined binary expression: {t1} {operator} {t2}")
            }
            CompileError::ProcessInGlobal => {
                panic!("process statements found in global.")
            }
            CompileError::ReturnTypeMismatch { expected, returned } => {
                panic!("return type mismatch. expected: {expected}, returned: {returned}.")
            }
            CompileError::ConditionNeedBool => {
                panic!("condition is supposed to be bool type.")
            }
            CompileError::AssignTypeMismatch {
                lvalue_type: declared_type,
                expression_type,
            } => {
                panic!("assignment type mismatch. declared: {declared_type}, expression type: {expression_type}.")
            }
            CompileError::CallArgumentTypesMismatch {
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
                );
            }
            CompileError::IllegalCast { from_type, to_type } => {
                panic!("illegal cast from {from_type} to {to_type}")
            }
            CompileError::UndeclaredMainFunction => {
                panic!("main function not declared in input document.")
            }
            CompileError::IllegalMainFunctionType => {
                panic!("the type of main function must be () -> i32.")
            }

            // Ir Errors
            CompileError::BuiltinFileNotExist => {
                panic!("builtin file not exist.");
            }
            CompileError::BuiltinFunctionFileNotExist(identifier) => {
                panic!("builtin function {identifier} file not exist")
            }

            // Scope Errors
            CompileError::NullScope => {
                panic!("null scope.");
            }
            CompileError::ScopeMismatch {
                expected,
                encountered,
            } => {
                panic!("scope mismatch. expected {expected}, encountered {encountered}.");
            }
            CompileError::DuplicateIdentifierInSameScope(identifier) => {
                panic!("encountered duplicate identifier {identifier} in same scope.")
            }
            CompileError::NotInFunction => {
                panic!("outside any function.")
            }
        }
    }
}
