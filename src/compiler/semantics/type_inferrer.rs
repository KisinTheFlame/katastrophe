use std::collections::HashMap;
use std::rc::Rc;

use crate::CompileResult;
use crate::compiler::bit_width::BitWidth;
use crate::compiler::context::Context;
use crate::compiler::context::DocumentId;
use crate::compiler::err::CompileError;
use crate::compiler::err::IceUnwrap;
use crate::compiler::scope::Scope;
use crate::compiler::scope::Tag;
use crate::compiler::syntax::ast::Document;
use crate::compiler::syntax::ast::crumb::Field;
use crate::compiler::syntax::ast::crumb::FieldInit;
use crate::compiler::syntax::ast::crumb::FunctionPrototype;
use crate::compiler::syntax::ast::crumb::Mutability;
use crate::compiler::syntax::ast::crumb::Parameter;
use crate::compiler::syntax::ast::crumb::Variable;
use crate::compiler::syntax::ast::expression::Expression;
use crate::compiler::syntax::ast::operator::Binary;
use crate::compiler::syntax::ast::operator::Unary;
use crate::compiler::syntax::ast::package::UsingPath;
use crate::compiler::syntax::ast::reference::Reference;
use crate::compiler::syntax::ast::statement::DefineDetail;
use crate::compiler::syntax::ast::statement::IfDetail;
use crate::compiler::syntax::ast::statement::LetDetail;
use crate::compiler::syntax::ast::statement::Statement;
use crate::compiler::syntax::ast::statement::StructDetail;
use crate::compiler::syntax::ast::statement::WhileDetail;
use crate::compiler::syntax::ast::ty::Type;
use crate::sys_error;
use crate::util::common::Array;

type ReferenceScope = Scope<Rc<Reference>>;

pub struct TypeInferrer {
    unary_operation_type_map: HashMap<(Unary, Rc<Type>), Rc<Type>>,
    binary_operation_type_map: HashMap<(Binary, Rc<Type>, Rc<Type>), Rc<Type>>,
    scope: ReferenceScope,
}

impl TypeInferrer {
    #[must_use]
    pub fn new() -> TypeInferrer {
        TypeInferrer {
            unary_operation_type_map: HashMap::new(),
            binary_operation_type_map: HashMap::new(),
            scope: ReferenceScope::new(),
        }
    }

    fn infer_binary_operation_type(&self, index: &(Binary, Rc<Type>, Rc<Type>)) -> CompileResult<Rc<Type>> {
        let (operator, left_type, right_type) = index;
        self.binary_operation_type_map
            .get(index)
            .cloned()
            .ok_or(CompileError::UndefinedBinaryExpression(
                *operator,
                left_type.clone(),
                right_type.clone(),
            ))
    }

    fn infer_unary_operation_type(&self, index: &(Unary, Rc<Type>)) -> CompileResult<Rc<Type>> {
        let (operator, ty) = index;
        self.unary_operation_type_map
            .get(index)
            .cloned()
            .ok_or(CompileError::UndefinedUnaryExpression(*operator, ty.clone()))
    }

    fn infer_assignment(&self, lvalue: &Rc<Expression>, expression: &Rc<Expression>) -> CompileResult<Rc<Expression>> {
        let (lvalue, lvalue_type) = self.infer_expression(lvalue)?;
        let (expression, expression_type) = self.infer_expression(expression)?;
        if *lvalue_type != *expression_type {
            return Err(CompileError::AssignTypeMismatch {
                lvalue_type,
                expression_type,
            });
        }
        Ok(Expression::Binary(Binary::Assign, Some(lvalue_type), lvalue, expression).into())
    }

    fn infer_spawn(
        &self,
        name: &Rc<String>,
        fields: &Rc<[Rc<FieldInit>]>,
    ) -> Result<(Rc<Expression>, Rc<Type>), CompileError> {
        let reference = self.scope.lookup(name);
        let reference = reference.as_ref().map(Rc::as_ref);
        let Some(Reference::StructDef(struct_id, _, field_defs)) = reference else {
            return Err(CompileError::UnknownType(name.clone()));
        };
        let mut field_map = field_defs
            .iter()
            .map(Rc::as_ref)
            .enumerate()
            .map(|(i, Field(field_name, field_type))| (field_name.clone(), (field_type.clone(), i)))
            .collect::<HashMap<_, _>>();
        let mut fields = fields
            .iter()
            .map(Rc::as_ref)
            .map(|FieldInit(field_name, field_expression)| {
                let Some((expected_type, index)) = field_map.get(field_name).cloned() else {
                    return Err(CompileError::FieldNotExist(field_name.clone()));
                };
                field_map.remove(field_name);
                let (field_expression, actual_type) = self.infer_expression(field_expression)?;
                if expected_type != actual_type {
                    return Err(CompileError::FieldTypeNotMatch {
                        field_name: field_name.clone(),
                        expected_type,
                        actual_type,
                    });
                }
                let field = FieldInit(field_name.clone(), field_expression);
                Ok((Rc::new(field), index))
            })
            .collect::<Result<Vec<_>, _>>()?;
        fields.sort_by_key(|(_, index)| *index);
        let fields = fields.into_iter().map(|(field, _)| field.clone()).collect::<Array<_>>();
        if !field_map.is_empty() {
            let missing_fields = field_map.keys().cloned().collect();
            return Err(CompileError::FieldMissing(missing_fields));
        }
        let spawn_type = Type::Struct {
            id: *struct_id,
            name: name.clone(),
            fields: field_defs.clone(),
        };
        Ok((
            Expression::StructSpawn(name.clone(), fields.clone()).into(),
            spawn_type.into(),
        ))
    }

    fn infer_access(
        &self,
        expression: &Rc<Expression>,
        field: &Rc<String>,
    ) -> Result<(Rc<Expression>, Rc<Type>), CompileError> {
        let (expression, struct_type) = self.infer_expression(expression)?;
        let Type::Struct { id: _, name: _, fields } = struct_type.as_ref() else {
            return Err(CompileError::AccessTargetNotStruct(field.clone()));
        };
        let field_type = fields
            .iter()
            .map(Rc::as_ref)
            .filter(|Field(field_name, _)| field_name == field)
            .map(|Field(_, field_type)| field_type)
            .next();
        let Some(field_type) = field_type else {
            return Err(CompileError::FieldNotExist(field.clone()));
        };
        Ok((
            Expression::Access(expression, Some(struct_type.clone()), field.clone()).into(),
            field_type.clone(),
        ))
    }

    fn infer_expression(&self, expression: &Rc<Expression>) -> CompileResult<(Rc<Expression>, Rc<Type>)> {
        let result: (Rc<Expression>, Rc<Type>) = match expression.as_ref() {
            Expression::Identifier(identifier) => {
                let reference = self.scope.lookup(identifier);
                let reference = reference.as_ref().map(Rc::as_ref);
                let Some(Reference::Binding(Some(id_type), _)) = reference else {
                    return Err(CompileError::UndeclaredIdentifier(identifier.clone()));
                };
                (Expression::Identifier(identifier.clone()).into(), id_type.clone())
            }
            Expression::IntLiteral(value) => (Expression::IntLiteral(*value).into(), Type::Int(BitWidth::Bit32).into()),
            Expression::CharLiteral(literal) => (
                Expression::CharLiteral(*literal).into(),
                Type::Int(BitWidth::Bit8).into(),
            ),
            Expression::FloatLiteral(_) => return Err(CompileError::UnsupportedFeature("float literal")),
            Expression::BoolLiteral(value) => (Expression::BoolLiteral(*value).into(), Type::Bool.into()),
            Expression::Unary(operator, _, child) => {
                let (child, result_type) = self.infer_expression(child)?;
                let child_type = self.infer_unary_operation_type(&(*operator, result_type.clone()))?;
                (
                    Expression::Unary(*operator, Some(child_type), child).into(),
                    result_type,
                )
            }
            Expression::Binary(operator, _, left, right) => {
                if *operator == Binary::Assign {
                    let assignment = self.infer_assignment(left, right)?;
                    (assignment, Type::Never.into())
                } else {
                    let (left, left_type) = self.infer_expression(left)?;
                    let (right, right_type) = self.infer_expression(right)?;
                    let child_type = left_type.clone();
                    let result_type = self.infer_binary_operation_type(&(*operator, left_type, right_type))?;
                    (
                        Expression::Binary(*operator, Some(child_type), left, right).into(),
                        result_type,
                    )
                }
            }
            Expression::Call(function_id, arguments) => {
                let (arguments, argument_types) = arguments
                    .iter()
                    .map(|x| self.infer_expression(x))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .unzip::<_, _, Vec<_>, Vec<_>>();
                let arguments: Array<Expression> = arguments.into();
                let argument_types: Array<Type> = argument_types.into();
                let Some(function_type) = self.scope.lookup(function_id) else {
                    return Err(CompileError::UndeclaredIdentifier(function_id.clone()));
                };
                let Reference::Binding(Some(function_type), _) = function_type.as_ref() else {
                    return Err(CompileError::CallTargetNotFunction(function_id.clone()));
                };
                let Type::Function {
                    return_type,
                    parameter_types,
                } = function_type.as_ref()
                else {
                    return Err(CompileError::CallTargetNotFunction(function_id.clone()));
                };
                let result_type = if argument_types == *parameter_types {
                    return_type.clone()
                } else {
                    return Err(CompileError::CallArgumentTypesMismatch {
                        function_id: function_id.clone(),
                        parameter_types: parameter_types.clone(),
                        argument_types,
                    });
                };
                (Expression::Call(function_id.clone(), arguments).into(), result_type)
            }
            Expression::Cast(expression, _, to_type) => {
                let (expression, from_type) = self.infer_expression(expression)?;
                match (from_type.as_ref(), to_type.as_ref()) {
                    (Type::Int(_), Type::Int(_)) => {}
                    (_, _) => {
                        return Err(CompileError::IllegalCast {
                            from_type,
                            to_type: to_type.clone(),
                        });
                    }
                }
                (
                    Expression::Cast(expression, Some(from_type), to_type.clone()).into(),
                    to_type.clone(),
                )
            }
            Expression::StructSpawn(name, fields) => self.infer_spawn(name, fields)?,
            Expression::Access(expression, _, field) => self.infer_access(expression, field)?,
        };
        Ok(result)
    }

    fn infer_return_statement(&self, return_value: Option<Rc<Expression>>) -> CompileResult<Option<Rc<Expression>>> {
        let (return_value, return_type) = match return_value {
            Some(return_value) => {
                let (return_value, return_type) = self.infer_expression(&return_value)?;
                (Some(return_value), return_type)
            }
            None => (None, Type::Never.into()),
        };
        let function_name = self.scope.current_function();
        let Some(function_type) = self.scope.lookup(&function_name) else {
            sys_error!("current function must be declared in scope");
        };
        let Reference::Binding(Some(function_type), _) = function_type.as_ref() else {
            sys_error!("current function reference must be a binding with concrete type");
        };
        let Type::Function {
            return_type: expected_type,
            parameter_types: _,
        } = function_type.as_ref()
        else {
            sys_error!("current function binding must have function type");
        };
        if return_type == *expected_type {
            Ok(return_value)
        } else {
            Err(CompileError::ReturnTypeMismatch {
                expected: expected_type.clone(),
                returned: return_type,
            })
        }
    }

    fn infer_if_statement(
        &mut self,
        context: &Context,
        IfDetail {
            condition,
            true_body,
            false_body,
        }: &IfDetail,
    ) -> CompileResult<Statement> {
        let (condition, condition_type) = self.infer_expression(condition)?;
        if *condition_type != Type::Bool {
            return Err(CompileError::ConditionNeedBool);
        }
        let true_body = {
            let _true_scope = self.scope.enter(Tag::Anonymous);
            self.infer_statement(context, true_body)?
        };
        let false_body = match false_body {
            Some(false_body) => {
                let _false_scope = self.scope.enter(Tag::Anonymous);
                Some(self.infer_statement(context, false_body)?)
            }
            None => None,
        };
        Ok(Statement::If(IfDetail {
            condition,
            true_body,
            false_body,
        }))
    }

    fn infer_while_statement(
        &mut self,
        context: &Context,
        WhileDetail(condition, body): &WhileDetail,
    ) -> CompileResult<Statement> {
        let (condition, condition_type) = self.infer_expression(condition)?;
        if *condition_type != Type::Bool {
            return Err(CompileError::ConditionNeedBool);
        }
        let _while_scope = self.scope.enter(Tag::Named("while"));
        let body = self.infer_statement(context, body)?;
        Ok(Statement::While(WhileDetail(condition, body)))
    }

    fn infer_let_statement(
        &mut self,
        LetDetail(Variable(identifier, lvalue_type, mutability), expression): &LetDetail,
    ) -> CompileResult<Statement> {
        let (expression, expression_type) = self.infer_expression(expression)?;
        let lvalue_type = match lvalue_type {
            None => expression_type.clone(),
            Some(annotated) => annotated.clone(),
        };
        if lvalue_type != expression_type {
            return Err(CompileError::AssignTypeMismatch {
                lvalue_type,
                expression_type,
            });
        }
        let reference = Reference::Binding(Some(lvalue_type.clone()), *mutability).into();
        if self.scope.is_global() && !Self::is_supported_global_initializer(expression.as_ref()) {
            return Err(CompileError::GlobalInitializerNotConstant);
        }
        if self.scope.is_global() {
            self.scope.declare(identifier.clone(), reference)?;
        } else {
            self.scope.overwrite(identifier.clone(), reference);
        }
        Ok(Statement::Let(LetDetail(
            Variable(identifier.clone(), Some(lvalue_type), *mutability),
            expression,
        )))
    }

    fn is_supported_global_initializer(expression: &Expression) -> bool {
        matches!(
            expression,
            Expression::IntLiteral(_) | Expression::CharLiteral(_) | Expression::BoolLiteral(_)
        )
    }

    fn infer_define_statement(
        &mut self,
        context: &Context,
        prototype: &Rc<FunctionPrototype>,
        body: &Rc<Statement>,
    ) -> CompileResult<Statement> {
        let FunctionPrototype {
            identifier,
            parameters,
            function_type,
        } = prototype.as_ref();
        let _function_scope = self.scope.enter(Tag::Function(identifier.clone()));
        let Type::Function {
            return_type,
            parameter_types,
        } = function_type.as_ref()
        else {
            sys_error!("must be a function type.");
        };
        parameters
            .iter()
            .map(Rc::as_ref)
            .zip(parameter_types.iter())
            .try_for_each(|(Parameter(identifier), parameter_type)| {
                let reference = Reference::Binding(Some(parameter_type.clone()), Mutability::Immutable).into();
                self.scope.declare(identifier.clone(), reference)
            })?;
        let body = self.infer_statement(context, body)?;
        Ok(Statement::Define(DefineDetail {
            prototype: FunctionPrototype {
                identifier: identifier.clone(),
                parameters: parameters.clone(),
                function_type: Type::Function {
                    return_type: return_type.clone(),
                    parameter_types: parameter_types.clone(),
                }
                .into(),
            }
            .into(),
            builtin: false,
            body,
        }))
    }

    fn infer_statement(&mut self, context: &Context, statement: &Rc<Statement>) -> CompileResult<Rc<Statement>> {
        let result = match statement.as_ref() {
            Statement::Empty => Statement::Empty,
            Statement::Block(statements) => {
                let _block_scope = self.scope.enter(Tag::Anonymous);
                let statements = statements
                    .iter()
                    .map(|statement| self.infer_statement(context, statement))
                    .collect::<Result<Rc<_>, _>>()?;
                Statement::Block(statements)
            }
            Statement::Return(return_value) => Statement::Return(self.infer_return_statement(return_value.clone())?),
            Statement::Expression(expression) => {
                let (expression, _) = self.infer_expression(expression)?;
                Statement::Expression(expression)
            }
            Statement::If(if_detail) => self.infer_if_statement(context, if_detail)?,
            Statement::While(while_detail) => self.infer_while_statement(context, while_detail)?,
            Statement::Let(let_detail) => self.infer_let_statement(let_detail)?,
            Statement::Define(DefineDetail {
                prototype,
                builtin: true,
                body,
            }) => Statement::Define(DefineDetail {
                prototype: prototype.clone(),
                builtin: true,
                body: body.clone(),
            }),
            Statement::Define(DefineDetail {
                prototype,
                builtin: false,
                body,
            }) => self.infer_define_statement(context, prototype, body)?,
            Statement::Using(UsingPath(document_path, symbol)) => {
                let used_document_id = context
                    .id_map
                    .get(document_path)
                    .or_ice("using path should be registered during parsing");
                let reference_map = context
                    .reference_map
                    .get(used_document_id)
                    .or_ice("reference map should be populated for used document");
                let Some(reference) = reference_map.get(symbol) else {
                    sys_error!("used symbol must exist");
                };
                self.scope.declare(symbol.clone(), reference.clone())?;
                Statement::Using(UsingPath(document_path.clone(), symbol.clone()))
            }
            Statement::Struct(struct_detail) => Statement::Struct(struct_detail.clone()),
        };
        Ok(result.into())
    }

    fn pre_scan_def(&mut self, context: &Context, document_id: u32, func_name: &Rc<String>) -> CompileResult<()> {
        let reference = context
            .reference_map
            .get(&document_id)
            .or_ice("reference map should be populated during parsing")
            .get(func_name)
            .or_ice("function reference should be registered during parsing");
        self.scope.declare(func_name.clone(), reference.clone())?;
        Ok(())
    }

    fn pre_scan_struct(&mut self, name: Rc<String>, context: &Context, document_id: DocumentId) -> CompileResult<()> {
        let reference = context
            .reference_map
            .get(&document_id)
            .or_ice("reference map should be populated during parsing")
            .get(&name)
            .or_ice("struct reference should be registered during parsing");
        self.scope.declare(name, reference.clone())?;
        Ok(())
    }

    fn pre_scan_global_items(
        &mut self,
        document: &Document,
        context: &Context,
        document_id: DocumentId,
    ) -> CompileResult<()> {
        document
            .statements
            .iter()
            .try_for_each(|statement| match statement.as_ref() {
                Statement::Empty
                | Statement::Block(_)
                | Statement::Return(_)
                | Statement::Expression(_)
                | Statement::If(_)
                | Statement::While(_) => Err(CompileError::ProcessInGlobal),
                Statement::Let(_) | Statement::Using(_) => Ok(()),
                Statement::Struct(StructDetail { name, .. }) => {
                    self.pre_scan_struct(name.clone(), context, document_id)
                }
                Statement::Define(DefineDetail { prototype, .. }) => {
                    let FunctionPrototype { identifier, .. } = prototype.as_ref();
                    self.pre_scan_def(context, document_id, identifier)
                }
            })
    }

    fn init_binary_operator_type_map(&mut self) {
        for operator in [
            Binary::Add,
            Binary::Subtract,
            Binary::Multiply,
            Binary::Divide,
            Binary::BitAnd,
            Binary::BitOr,
        ] {
            for bit_width in BitWidth::all() {
                let t = Rc::new(Type::Int(*bit_width));
                self.binary_operation_type_map
                    .insert((operator, t.clone(), t.clone()), t);
            }
        }

        for operator in [Binary::LeftShift, Binary::RightShift] {
            for w1 in BitWidth::all() {
                for w2 in BitWidth::all() {
                    let left_type = Rc::new(Type::Int(*w1));
                    let right_type = Type::Int(*w2).into();
                    self.binary_operation_type_map
                        .insert((operator, left_type.clone(), right_type), left_type);
                }
            }
        }

        for operator in [
            Binary::Equal,
            Binary::NotEqual,
            Binary::LessThan,
            Binary::LessThanEqual,
            Binary::GreaterThan,
            Binary::GreaterThanEqual,
        ] {
            let bool_type = Rc::new(Type::Bool);
            for bit_width in BitWidth::all() {
                let t = Rc::new(Type::Int(*bit_width));
                self.binary_operation_type_map
                    .insert((operator, t.clone(), t.clone()), bool_type.clone());
            }
        }

        for operator in [Binary::LogicalAnd, Binary::LogicalOr] {
            self.binary_operation_type_map
                .insert((operator, Type::Bool.into(), Type::Bool.into()), Type::Bool.into());
        }
    }

    fn init_unary_operator_type_map(&mut self) {
        for bit_width in BitWidth::all() {
            let t = Rc::new(Type::Int(*bit_width));
            self.unary_operation_type_map
                .insert((Unary::BitNot, t.clone()), t.clone());
            self.unary_operation_type_map
                .insert((Unary::Negative, t.clone()), t.clone());
        }

        self.unary_operation_type_map
            .insert((Unary::LogicalNot, Type::Bool.into()), Type::Bool.into());
    }

    /// 解析所有顶层全局 `let` 的类型并回写 `context.reference_map`。
    /// 必须先于 `infer` 在所有文档上跑完，让跨文档 `using` 能看到对方的全局类型。
    /// parser 阶段未标注类型的全局 let 在 `reference_map` 里只占一个 `Binding(None)`，
    /// 这里把它覆盖成 `Binding(Some(t))`。
    ///
    /// # Errors
    pub fn resolve_global_let_types(&self, context: &mut Context, id: DocumentId) -> CompileResult<()> {
        let Some(document) = context.document_map.get(&id) else {
            sys_error!("document must exist");
        };
        let resolved = document
            .statements
            .iter()
            .filter_map(|statement| match statement.as_ref() {
                Statement::Let(LetDetail(Variable(name, lvalue_type, mutability), expression)) => {
                    let ty = match lvalue_type {
                        Some(t) => Some(t.clone()),
                        None => derive_literal_type(expression),
                    };
                    ty.map(|ty| (name.clone(), ty, *mutability))
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        let entry = context.reference_map.entry(id).or_default();
        for (name, ty, mutability) in resolved {
            entry.insert(name, Reference::Binding(Some(ty), mutability).into());
        }
        Ok(())
    }

    /// # Errors
    pub fn infer(&mut self, context: &mut Context, id: DocumentId) -> CompileResult<()> {
        let Some(document) = context.document_map.remove(&id) else {
            sys_error!("document must exist");
        };
        self.init_unary_operator_type_map();
        self.init_binary_operator_type_map();
        let _global_scope = self.scope.enter(Tag::Global);
        self.pre_scan_global_items(&document, context, id)?;
        let statements = document
            .statements
            .iter()
            .map(|statement| self.infer_statement(context, statement))
            .collect::<Result<Rc<_>, _>>()?;
        context.document_map.insert(id, Document { statements });
        Ok(())
    }
}

/// 从全局 `let` 的初始化表达式推断类型。仅覆盖语言允许作为全局初始化的字面量；
/// 非字面量返回 `None`，由后续完整类型推断在 `infer_let_statement` 报
/// `GlobalInitializerNotConstant`。
fn derive_literal_type(expression: &Expression) -> Option<Rc<Type>> {
    match expression {
        Expression::IntLiteral(_) => Some(Type::Int(BitWidth::Bit32).into()),
        Expression::CharLiteral(_) => Some(Type::Int(BitWidth::Bit8).into()),
        Expression::BoolLiteral(_) => Some(Type::Bool.into()),
        _ => None,
    }
}

impl Default for TypeInferrer {
    fn default() -> Self {
        Self::new()
    }
}
