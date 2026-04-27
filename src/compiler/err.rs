use std::fmt;
use std::fmt::Display;
use std::rc::Rc;

use crate::util::common::Array;

use super::lexis::token::Keyword;
use super::lexis::token::Symbol;
use super::lexis::token::Token;
use super::syntax::ast::crumb::Identifier;
use super::syntax::ast::operator::Binary;
use super::syntax::ast::operator::Unary;
use super::syntax::ast::ty::Type;

#[macro_export]
macro_rules! sys_error {
    ($($arg:tt)*) => {
        panic!("编译器内部错误: {}", format!($($arg)*))
    };
}

/// 用于把"理论上不可能为 None"的 `Option`（编译器内部不变量）展开为值；
/// 万一不变量被破坏，以编译器内部错误的形式 panic，比裸 `.unwrap()`
/// 给出更有用的定位信息。
pub trait IceUnwrap<T> {
    fn or_ice(self, msg: &str) -> T;
}

impl<T> IceUnwrap<T> for Option<T> {
    fn or_ice(self, msg: &str) -> T {
        self.unwrap_or_else(|| sys_error!("{msg}"))
    }
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

    UnknownType(Rc<Identifier>),
    UnknownPackage {
        path: String,
    },
    BuiltinNotAllowedOutsideStd(Rc<Identifier>),

    // Semantics Errors
    UndeclaredIdentifier(Rc<Identifier>),
    IllegalLValue,
    AssigningImmutableVariable(Rc<Identifier>),

    // Type Errors
    CallTargetNotFunction(Rc<Identifier>),
    AccessTargetNotStruct(Rc<Identifier>),
    FieldNotExist(Rc<Identifier>),
    FieldTypeNotMatch {
        field_name: Rc<Identifier>,
        expected_type: Rc<Type>,
        actual_type: Rc<Type>,
    },
    FieldMissing(Array<Identifier>),

    UndefinedUnaryExpression(Unary, Rc<Type>),
    UndefinedBinaryExpression(Binary, Rc<Type>, Rc<Type>),
    ProcessInGlobal,
    GlobalInitializerNotConstant,

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
        parameter_types: Array<Type>,
        argument_types: Array<Type>,
    },
    IllegalCast {
        from_type: Rc<Type>,
        to_type: Rc<Type>,
    },
    UnsupportedFeature(&'static str),

    UndeclaredMainFunction,
    IllegalMainFunctionType,

    // Ir Errors
    BuiltinFunctionFileNotExist(Rc<Identifier>),

    // Environment Errors
    FileReadFailed {
        path: String,
        error: String,
    },
    EntryResolveFailed {
        path: String,
        error: String,
    },
    FileWriteFailed {
        path: String,
        error: String,
    },
    ExternalCommandFailed {
        command: &'static str,
        error: String,
    },
    LinkFailed(String),

    DuplicateIdentifierInSameScope(Rc<Identifier>),
}

impl Display for CompileError {
    #[allow(clippy::too_many_lines)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Lex Errors
            CompileError::IllegalIntegerLiteral(ref s) | CompileError::IllegalFloatLiteral(ref s) => {
                write!(f, "encountering illegal literal: {s}")
            }
            CompileError::IllegalEscapeChar(c) => {
                write!(f, "illegal escape char: {c}")
            }
            CompileError::UnexpectedCharacter(c) => {
                write!(f, "encountering unexpected character: {c}")
            }
            CompileError::UnexpectedLexEOF => write!(f, "code should not end here"),

            // Parse Errors
            CompileError::MissingIdentifier => {
                write!(f, "failed to expect an identifier")
            }
            CompileError::MissingKeyword(keyword) => {
                write!(f, "missing expected keyword: {keyword:?}")
            }
            CompileError::MissingSymbol(symbol) => {
                write!(f, "missing expected symbol: {symbol:?}")
            }
            CompileError::UnexpectedParseEOF => {
                write!(f, "encountering unexpected EOF")
            }
            CompileError::UnexpectedToken(token) => {
                write!(f, "encountering unexpected token: {token:?}")
            }
            CompileError::UnknownType(unknown_type) => {
                write!(f, "encountering unknown type: {unknown_type}")
            }
            CompileError::UnknownPackage { path } => {
                write!(f, "unknown package, looked up: {path}")
            }
            CompileError::BuiltinNotAllowedOutsideStd(identifier) => {
                write!(f, "builtin function '{identifier}' is only allowed in std packages")
            }

            // Semantics Errors
            CompileError::UndeclaredIdentifier(identifier) => {
                write!(f, "undeclared identifier: {identifier}.")
            }
            CompileError::IllegalLValue => {
                write!(f, "illegal lvalue.")
            }
            CompileError::AssigningImmutableVariable(identifier) => {
                write!(f, "assigning immutable variable: {identifier}.")
            }

            // Type Errors
            CompileError::CallTargetNotFunction(identifier) => {
                write!(f, "call target '{identifier}' is not a function.")
            }
            CompileError::AccessTargetNotStruct(field) => {
                write!(f, "cannot access field '{field}' on a non-struct value.")
            }
            CompileError::FieldNotExist(field_name) => {
                write!(f, "field '{field_name}' does not exists.")
            }
            CompileError::FieldTypeNotMatch {
                field_name,
                expected_type,
                actual_type,
            } => {
                write!(
                    f,
                    "field {field_name} type mismatch. expected type: {expected_type}, actual type: {actual_type}"
                )
            }
            CompileError::FieldMissing(missing_fields) => {
                let missing_fields = missing_fields
                    .iter()
                    .map(Rc::as_ref)
                    .cloned()
                    .collect::<Rc<_>>()
                    .join(", ");
                write!(f, "missing field(s): {missing_fields}")
            }
            CompileError::UndefinedUnaryExpression(operator, ty) => {
                write!(f, "undefined unary expression: {operator} {ty}")
            }
            CompileError::UndefinedBinaryExpression(operator, t1, t2) => {
                write!(f, "undefined binary expression: {t1} {operator} {t2}")
            }
            CompileError::ProcessInGlobal => {
                write!(f, "process statements found in global.")
            }
            CompileError::GlobalInitializerNotConstant => {
                write!(f, "global initializer must be a supported constant expression.")
            }
            CompileError::ReturnTypeMismatch { expected, returned } => {
                write!(f, "return type mismatch. expected: {expected}, returned: {returned}.")
            }
            CompileError::ConditionNeedBool => {
                write!(f, "condition is supposed to be bool type.")
            }
            CompileError::AssignTypeMismatch {
                lvalue_type: declared_type,
                expression_type,
            } => {
                write!(
                    f,
                    "assignment type mismatch. declared: {declared_type}, expression type: {expression_type}."
                )
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
                write!(
                    f,
                    "arguments of call statement mismatch with parameters.
                function: {function_id},
                parameter types: {parameter_types},
                argument types: {argument_types}"
                )
            }
            CompileError::IllegalCast { from_type, to_type } => {
                write!(f, "illegal cast from {from_type} to {to_type}")
            }
            CompileError::UnsupportedFeature(feature) => {
                write!(f, "unsupported feature: {feature}")
            }
            CompileError::UndeclaredMainFunction => {
                write!(f, "main function not declared in input document.")
            }
            CompileError::IllegalMainFunctionType => {
                write!(f, "the type of main function must be () -> i32.")
            }

            // Ir Errors
            CompileError::BuiltinFunctionFileNotExist(identifier) => {
                write!(f, "builtin function {identifier} file not exist")
            }

            // Environment Errors
            CompileError::FileReadFailed { path, error } => {
                write!(f, "failed to read file '{path}': {error}")
            }
            CompileError::EntryResolveFailed { path, error } => {
                write!(f, "failed to resolve entry file '{path}': {error}")
            }
            CompileError::FileWriteFailed { path, error } => {
                write!(f, "failed to write file '{path}': {error}")
            }
            CompileError::ExternalCommandFailed { command, error } => {
                write!(f, "failed to run external command '{command}': {error}")
            }
            CompileError::LinkFailed(error) => {
                write!(f, "failed to link executable:\n{error}")
            }

            CompileError::DuplicateIdentifierInSameScope(identifier) => {
                write!(f, "encountered duplicate identifier {identifier} in same scope.")
            }
        }
    }
}
