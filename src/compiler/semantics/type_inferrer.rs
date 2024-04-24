use std::collections::HashMap;

use crate::{
    compiler::{
        context::{Context, DocumentId},
        err::CompileError,
        scope::{Scope, Tag},
        syntax::ast::{
            crumb::{FunctionPrototype, Identifier, Parameter, Variable},
            expression::Expression,
            operator::{Binary, Unary},
            package::{DocumentPath, UsingPath},
            statement::{DefineDetail, IfDetail, LetDetail, Statement, WhileDetail},
            ty::Type,
            Document,
        },
    },
    system_error,
};

use super::err::{SemanticError, TypeError};

type TypeScope = Scope<Type>;

pub struct TypeInferrer {
    unary_operation_type_map: HashMap<(Unary, Type), Type>,
    binary_operation_type_map: HashMap<(Binary, Type, Type), Type>,
    scope: TypeScope,
}

impl TypeInferrer {
    #[must_use]
    pub fn new() -> TypeInferrer {
        TypeInferrer {
            unary_operation_type_map: HashMap::new(),
            binary_operation_type_map: HashMap::new(),
            scope: TypeScope::new(),
        }
    }

    fn infer_binary_operation_type(
        &self,
        index: &(Binary, Type, Type),
    ) -> Result<Type, CompileError> {
        self.binary_operation_type_map
            .get(index)
            .cloned()
            .ok_or(TypeError::UndefinedOperation.into())
    }

    fn infer_unary_operation_type(&self, index: &(Unary, Type)) -> Result<Type, CompileError> {
        self.unary_operation_type_map
            .get(index)
            .cloned()
            .ok_or(TypeError::UndefinedOperation.into())
    }

    fn infer_lvalue(&self, lvalue: &Expression) -> Result<Type, CompileError> {
        match lvalue {
            Expression::Identifier(identifier) => {
                if let Some(var_type) = self.scope.lookup(identifier)? {
                    Ok(var_type)
                } else {
                    Err(SemanticError::UndeclaredIdentifier(identifier.clone()).into())
                }
            }
            _ => Err(SemanticError::IllegalLValue.into()),
        }
    }

    fn infer_assignment(
        &self,
        sub_type: &mut Type,
        lvalue: &Expression,
        expression: &mut Expression,
    ) -> Result<Type, CompileError> {
        let lvalue_type = self.infer_lvalue(lvalue)?;
        let expression_type = self.infer_expression(expression)?;
        if lvalue_type != expression_type {
            return Err(TypeError::AssignTypeMismatch {
                lvalue_type,
                expression_type,
            }
            .into());
        }
        *sub_type = lvalue_type;
        Ok(Type::Never)
    }

    fn infer_expression(&self, expression: &mut Expression) -> Result<Type, CompileError> {
        let result_type = match expression {
            Expression::Identifier(identifier) => {
                let id_type = self.scope.lookup(identifier)?;
                let Some(id_type) = id_type else {
                    return Err(SemanticError::UndeclaredIdentifier(identifier.clone()).into());
                };
                id_type
            }
            Expression::IntLiteral(_) => Type::I32,
            Expression::FloatLiteral(_) => todo!(),
            Expression::BoolLiteral(_) => Type::Bool,
            Expression::Unary(operator, sub_type, expression) => {
                *sub_type = self.infer_expression(expression)?;
                self.infer_unary_operation_type(&(*operator, sub_type.clone()))?
            }
            Expression::Binary(operator, sub_type, left, right) => {
                if *operator == Binary::Assign {
                    self.infer_assignment(sub_type, left, right)?
                } else {
                    let left_type = self.infer_expression(left)?;
                    let right_type = self.infer_expression(right)?;
                    *sub_type = left_type.clone();
                    self.infer_binary_operation_type(&(operator.clone(), left_type, right_type))?
                }
            }
            Expression::Call(function_id, arguments) => {
                let argument_types = arguments
                    .iter_mut()
                    .map(|x| self.infer_expression(x))
                    .collect::<Result<Vec<Type>, CompileError>>()?;
                let Some(function_type) = self.scope.lookup(function_id)? else {
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
            .map_or(Ok(Type::Never), |v| self.infer_expression(v))?;
        let function_name = self.scope.current_function()?;
        let function_type = self.scope.lookup(&function_name)?.unwrap();
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

    fn infer_statement(
        &mut self,
        id_map: &HashMap<DocumentPath, DocumentId>,
        type_map: &HashMap<DocumentId, HashMap<Identifier, Type>>,
        statement: &mut Statement,
    ) -> Result<(), CompileError> {
        match statement {
            Statement::Empty => Ok(()),
            Statement::Block(statements) => {
                self.scope.enter(Tag::Anonymous);
                statements
                    .iter_mut()
                    .try_for_each(|statement| self.infer_statement(id_map, type_map, statement))?;
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
                    return Err(TypeError::ConditionNeedBool.into());
                }
                self.scope.enter(Tag::Anonymous);
                self.infer_statement(id_map, type_map, true_body)?;
                self.scope.leave(Tag::Anonymous)?;

                if let Some(false_body) = false_body {
                    self.scope.enter(Tag::Anonymous);
                    self.infer_statement(id_map, type_map, false_body)?;
                    self.scope.leave(Tag::Anonymous)?;
                }
                Ok(())
            }
            Statement::While(WhileDetail(condition, body)) => {
                if self.infer_expression(condition)? != Type::Bool {
                    return Err(TypeError::ConditionNeedBool.into());
                }
                self.scope.enter(Tag::Named("while"));
                self.infer_statement(id_map, type_map, body)?;
                self.scope.leave(Tag::Named("while"))?;
                Ok(())
            }
            Statement::Let(LetDetail(Variable(identifier, lvalue_type, _), expression)) => {
                let expression_type = self.infer_expression(expression)?;

                if *lvalue_type == Type::Unknown {
                    *lvalue_type = expression_type.clone();
                }

                let lvalue_type = lvalue_type.clone();
                if lvalue_type == expression_type {
                    if self.scope.is_global()? {
                        self.scope
                            .declare(identifier.clone(), lvalue_type.clone())?;
                    } else {
                        self.scope
                            .overwrite(identifier.clone(), lvalue_type.clone())?;
                    }
                    Ok(())
                } else {
                    Err(TypeError::AssignTypeMismatch {
                        lvalue_type,
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
                builtin,
                body,
            }) => {
                if *builtin {
                    return Ok(());
                }
                self.scope.enter(Tag::Function(identifier.clone()));

                let Type::Function {
                    return_type: _,
                    parameter_types,
                } = function_type
                else {
                    return Err(system_error!("must be a function type."));
                };
                parameters.iter().zip(parameter_types).try_for_each(
                    |(Parameter(identifier), parameter_type)| {
                        self.scope
                            .declare(identifier.clone(), parameter_type.clone())
                    },
                )?;

                self.infer_statement(id_map, type_map, body)?;

                self.scope.leave(Tag::Function(identifier.clone()))?;
                Ok(())
            }
            Statement::Using(UsingPath(document_path, symbol)) => {
                let used_document_id = id_map.get(document_path).unwrap();
                let Some(symbol_type) = type_map.get(used_document_id).unwrap().get(symbol) else {
                    return Err(system_error!("used symbol must exist"));
                };
                self.scope.declare(symbol.clone(), symbol_type.clone())?;
                Ok(())
            }
        }
    }

    fn pre_scan_function_prototype(
        &mut self,
        FunctionPrototype {
            identifier,
            parameters: _,
            function_type,
        }: &FunctionPrototype,
    ) -> Result<(), CompileError> {
        self.scope
            .declare(identifier.clone(), function_type.clone())?;
        Ok(())
    }

    fn pre_scan_global_items(&mut self, document: &Document) -> Result<(), CompileError> {
        document
            .statements
            .iter()
            .try_for_each(|statement| match statement {
                Statement::Empty
                | Statement::Block(_)
                | Statement::Return(_)
                | Statement::Expression(_)
                | Statement::If(_)
                | Statement::While(_) => Err(TypeError::ProcessInGlobal.into()),
                Statement::Let(_) | Statement::Using(_) => Ok(()),
                Statement::Define(define_detail) => {
                    self.pre_scan_function_prototype(&define_detail.prototype)
                }
            })
    }

    fn init_binary_operator_type_map(&mut self) {
        for operator in [
            Binary::Add,
            Binary::Subtract,
            Binary::Multiply,
            Binary::Divide,
        ] {
            self.binary_operation_type_map
                .insert((operator, Type::I32, Type::I32), Type::I32);
        }

        for operator in [
            Binary::Equal,
            Binary::NotEqual,
            Binary::LessThan,
            Binary::LessThanEqual,
            Binary::GreaterThan,
            Binary::GreaterThanEqual,
        ] {
            self.binary_operation_type_map
                .insert((operator, Type::I32, Type::I32), Type::Bool);
        }

        for operator in [Binary::LogicalAnd, Binary::LogicalOr] {
            self.binary_operation_type_map
                .insert((operator, Type::Bool, Type::Bool), Type::Bool);
        }
    }

    fn init_unary_operator_type_map(&mut self) {
        self.unary_operation_type_map
            .insert((Unary::BitNot, Type::I32), Type::I32);
        self.unary_operation_type_map
            .insert((Unary::LogicalNot, Type::Bool), Type::Bool);
        self.unary_operation_type_map
            .insert((Unary::Negative, Type::I32), Type::I32);
    }

    /// # Errors
    pub fn infer(&mut self, context: &mut Context, id: DocumentId) -> Result<(), CompileError> {
        let document = context.document_map.get_mut(&id).unwrap();
        self.init_unary_operator_type_map();
        self.init_binary_operator_type_map();
        self.scope.enter(Tag::Global);
        self.pre_scan_global_items(document)?;
        document.statements.iter_mut().try_for_each(|statement| {
            self.infer_statement(&context.id_map, &context.type_map, statement)
        })?;
        self.scope.leave(Tag::Global)?;
        Ok(())
    }
}
