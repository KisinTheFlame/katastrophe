use std::collections::HashMap;

use crate::compiler::{
    err::CompileError,
    scope::{Scope, Tag},
    syntax::ast::{
        BinaryOperator, DefineDetail, Expression, FunctionPrototype, IfDetail, LetDetail,
        Parameter, Program, Statement, Type, Variable,
    },
};

use super::err::{SemanticError, TypeError};

type TypeScope = Scope<Type>;

pub struct TypeInferrer {
    binary_operation_type_map: HashMap<(BinaryOperator, Type, Type), Type>,
    scope: TypeScope,
}

impl TypeInferrer {
    #[must_use]
    pub fn new() -> TypeInferrer {
        TypeInferrer {
            binary_operation_type_map: HashMap::new(),
            scope: TypeScope::new(),
        }
    }

    fn infer_binary_operation_type(
        &self,
        index: &(BinaryOperator, Type, Type),
    ) -> Result<Type, CompileError> {
        self.binary_operation_type_map
            .get(index)
            .cloned()
            .ok_or(TypeError::NoSuchOperator.into())
    }

    fn infer_expression(&self, expression: &mut Expression) -> Result<Type, CompileError> {
        let result_type = match expression {
            Expression::Identifier(identifier) => {
                let id_type = self.scope.lookup_symbol(identifier)?;
                let Some(id_type) = id_type else {
                    return Err(SemanticError::UndeclaredIdentifier(identifier.clone()).into());
                };
                id_type
            }
            Expression::IntLiteral(_) => Type::I32,
            Expression::FloatLiteral(_) => todo!(),
            Expression::Unary(_, _, _) => todo!(),
            Expression::Binary(operator, sub_type, left, right) => {
                let left_type = self.infer_expression(left)?;
                let right_type = self.infer_expression(right)?;
                if *sub_type == Type::Unknown {
                    *sub_type = left_type.clone();
                }
                self.infer_binary_operation_type(&(operator.clone(), left_type, right_type))?
            }
            Expression::Call(function_id, arguments) => {
                let argument_types = arguments
                    .iter_mut()
                    .map(|x| self.infer_expression(x))
                    .collect::<Result<Vec<Type>, CompileError>>()?;
                let Some(function_type) = self.scope.lookup_symbol(function_id)? else {
                    return Err(SemanticError::UndeclaredIdentifier(function_id.clone()).into());
                };
                let Type::Function {
                    return_type,
                    parameter_types,
                } = function_type
                else {
                    return Err(TypeError::ShouldBeFunctionType.into());
                };
                if argument_types == parameter_types {
                    *return_type
                } else {
                    return Err(TypeError::CallArgumentTypesMismatch {
                        function_id: function_id.clone(),
                        parameter_types,
                        argument_types,
                    }
                    .into());
                }
            }
        };
        Ok(result_type)
    }

    fn infer_return_statement(
        &self,
        return_value: &mut Option<Expression>,
    ) -> Result<(), CompileError> {
        let return_type = return_value
            .as_mut()
            .map_or(Ok(Type::Void), |v| self.infer_expression(v))?;
        let function_name = self.scope.get_current_function_name()?;
        let function_type = self.scope.lookup_symbol(&function_name)?.unwrap();
        let Type::Function {
            return_type: expected_type,
            parameter_types: _,
        } = function_type
        else {
            return Err(TypeError::ShouldBeFunctionType.into());
        };
        if return_type == *expected_type {
            Ok(())
        } else {
            Err(TypeError::ReturnTypeMismatch {
                expected: *expected_type,
                returned: return_type,
            }
            .into())
        }
    }

    fn infer_statement(&mut self, statement: &mut Statement) -> Result<(), CompileError> {
        match statement {
            Statement::Empty => Ok(()),
            Statement::Block(statements) => {
                self.scope.enter(Tag::Anonymous);
                statements
                    .iter_mut()
                    .try_for_each(|statement| self.infer_statement(statement))?;
                self.scope.leave(Tag::Anonymous)?;
                Ok(())
            }
            Statement::Return(return_value) => self.infer_return_statement(return_value),
            Statement::Expression(expression) => {
                self.infer_expression(expression)?;
                Ok(())
            }
            Statement::If(IfDetail {
                condition,
                true_body,
                false_body,
            }) => {
                if self.infer_expression(condition)? != Type::Bool {
                    return Err(TypeError::IfConditionNeedBool.into());
                }
                self.infer_statement(true_body)?;
                if let Some(false_body) = false_body {
                    self.infer_statement(false_body)?;
                }
                Ok(())
            }
            Statement::Let(LetDetail(Variable(identifier, var_type), expression)) => {
                let expression_type = self.infer_expression(expression)?;
                if *var_type == Type::Unknown {
                    *var_type = expression_type;
                    self.scope.declare(identifier.clone(), var_type.clone())?;
                    return Ok(());
                }

                let var_type = var_type.clone();
                if var_type == expression_type {
                    self.scope.declare(identifier.clone(), var_type)?;
                    Ok(())
                } else {
                    Err(TypeError::LetAssignTypeMismatch {
                        declared_type: var_type,
                        expression_type,
                    }
                    .into())
                }
            }
            Statement::Define(DefineDetail {
                prototype:
                    FunctionPrototype {
                        identifier,
                        parameters,
                        function_type,
                    },
                body,
            }) => {
                self.scope.enter(Tag::Function(identifier.clone()));

                let Type::Function {
                    return_type: _,
                    parameter_types,
                } = function_type
                else {
                    return Err(TypeError::ShouldBeFunctionType.into());
                };
                parameters.iter().zip(parameter_types).try_for_each(
                    |(Parameter(identifier), parameter_type)| {
                        self.scope
                            .declare(identifier.clone(), parameter_type.clone())
                    },
                )?;

                self.infer_statement(body)?;

                self.scope.leave(Tag::Function(identifier.clone()))?;
                Ok(())
            }
        }
    }

    fn pre_scan_function_prototype(
        &mut self,
        prototype: &FunctionPrototype,
    ) -> Result<(), CompileError> {
        self.scope.declare(
            prototype.identifier.clone(),
            prototype.function_type.clone(),
        )?;
        Ok(())
    }

    fn pre_scan_global_variable(&mut self, let_detail: &LetDetail) -> Result<(), CompileError> {
        let LetDetail(Variable(identifier, var_type), _) = let_detail;
        self.scope.declare(identifier.clone(), var_type.clone())?;
        Ok(())
    }

    fn pre_scan_global_items(&mut self, program: &Program) -> Result<(), CompileError> {
        program
            .statements
            .iter()
            .try_for_each(|statement| match statement {
                Statement::Empty
                | Statement::Block(_)
                | Statement::Return(_)
                | Statement::Expression(_)
                | Statement::If(_) => Err(TypeError::ProcessInGlobal.into()),
                Statement::Let(let_detail) => self.pre_scan_global_variable(let_detail),
                Statement::Define(define_detail) => {
                    self.pre_scan_function_prototype(&define_detail.prototype)
                }
            })
    }

    fn init_binary_operator_type_map(&mut self) {
        for operator in [
            BinaryOperator::Add,
            BinaryOperator::Subtract,
            BinaryOperator::Multiply,
            BinaryOperator::Divide,
        ] {
            self.binary_operation_type_map
                .insert((operator, Type::I32, Type::I32), Type::I32);
        }

        for operator in [
            BinaryOperator::Equal,
            BinaryOperator::NotEqual,
            BinaryOperator::LessThan,
            BinaryOperator::LessThanEqual,
            BinaryOperator::GreaterThan,
            BinaryOperator::GreaterThanEqual,
        ] {
            self.binary_operation_type_map
                .insert((operator, Type::I32, Type::I32), Type::Bool);
        }

        for operator in [BinaryOperator::LogicalAnd, BinaryOperator::LogicalOr] {
            self.binary_operation_type_map
                .insert((operator, Type::Bool, Type::Bool), Type::Bool);
        }
    }

    /// # Errors
    pub fn infer(&mut self, program: &mut Program) -> Result<(), CompileError> {
        self.init_binary_operator_type_map();
        self.scope.enter(Tag::Global);
        self.pre_scan_global_items(program)?;
        program
            .statements
            .iter_mut()
            .try_for_each(|statement| self.infer_statement(statement))?;
        self.scope.leave(Tag::Global)?;
        Ok(())
    }
}

impl Default for TypeInferrer {
    fn default() -> Self {
        Self::new()
    }
}
