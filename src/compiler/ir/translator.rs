use std::cmp::Ordering;
use std::collections::HashMap;
use std::rc::Rc;

use crate::CompileResult;
use crate::compiler::context::Context;
use crate::compiler::context::DocumentId;
use crate::compiler::err::CompileError;
use crate::compiler::scope::Tag;
use crate::compiler::syntax::ast::Document;
use crate::compiler::syntax::ast::crumb::Field;
use crate::compiler::syntax::ast::crumb::FieldInit;
use crate::compiler::syntax::ast::crumb::FunctionPrototype;
use crate::compiler::syntax::ast::crumb::Identifier;
use crate::compiler::syntax::ast::crumb::Parameter;
use crate::compiler::syntax::ast::crumb::Variable;
use crate::compiler::syntax::ast::expression::Expression;
use crate::compiler::syntax::ast::lvalue::LValue;
use crate::compiler::syntax::ast::operator::Binary;
use crate::compiler::syntax::ast::operator::Unary;
use crate::compiler::syntax::ast::package::DocumentPath;
use crate::compiler::syntax::ast::package::UsingPath;
use crate::compiler::syntax::ast::package::get_builtin;
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

use super::id::next_anonymous_id;
use super::id::next_function_id;
use super::id::next_global_id;
use super::id::next_label_id;
use super::id::next_parameter_id;
use super::id::next_variable_id;
use super::instruction::Comparator;
use super::instruction::Instruction;
use super::instruction::IrBinaryOpcode;
use super::instruction::IrFunctionPrototype;
use super::instruction::IrReference;
use super::instruction::IrScope;
use super::instruction::ir_type::IrField;
use super::instruction::ir_type::IrType;
use super::instruction::ir_type::find_field;
use super::instruction::memory::Memory;
use super::instruction::value::Value;

pub struct Translator {
    document_path: Rc<DocumentPath>,
    scope: IrScope,
}

enum DeclType {
    Anonymous,
    Param(Rc<Identifier>, Rc<IrType>),
    Local(Rc<Identifier>, Rc<IrType>),
    AnonymousLocal,
    Global(Rc<Identifier>, Rc<IrType>),
    Function(Rc<Identifier>, Rc<IrType>),
    Label(&'static str),
}

impl Translator {
    #[must_use]
    pub fn new(document_path: Rc<DocumentPath>) -> Translator {
        Translator {
            document_path,
            scope: IrScope::new(),
        }
    }

    fn declare(&mut self, value_type: DeclType) -> CompileResult<Rc<Value>> {
        let result = match value_type {
            DeclType::Anonymous => {
                let id = next_anonymous_id();
                Value::Register(format!("t{id}")).into()
            }
            DeclType::Param(symbol, data_type) => {
                let id = next_parameter_id();
                let value = Rc::new(Value::Parameter(format!("p{id}.{symbol}")));
                self.scope
                    .declare(symbol, IrReference::Binding((value.clone(), data_type)))?;
                value
            }
            DeclType::AnonymousLocal => {
                let id = next_anonymous_id();
                let memory = Memory::Stack(format!("t{id}")).into();
                Value::Pointer(memory).into()
            }
            DeclType::Local(symbol, data_type) => {
                let id = next_variable_id();
                let memory = Memory::Stack(format!("v{id}.{symbol}")).into();
                let value = Rc::new(Value::Pointer(memory));
                self.scope
                    .overwrite(symbol, IrReference::Binding((value.clone(), data_type)))?;
                value
            }
            DeclType::Global(symbol, data_type) => {
                let id = next_global_id();
                let memory = Memory::Global(format!("g{id}.{symbol}")).into();
                let value = Rc::new(Value::Pointer(memory));
                self.scope
                    .declare(symbol, IrReference::Binding((value.clone(), data_type)))?;
                value
            }
            DeclType::Function(symbol, data_type) => {
                let id = next_function_id();
                let value = Rc::new(Value::Function(format!("f{id}.{symbol}")));
                self.scope
                    .declare(symbol, IrReference::Binding((value.clone(), data_type)))?;
                value
            }
            DeclType::Label(tag) => {
                let id = next_label_id();
                Value::Label(format!("l{id}.{tag}")).into()
            }
        };
        Ok(result)
    }

    fn translate_block_statement(
        &mut self,
        context: &Context,
        statements: &[Rc<Statement>],
    ) -> CompileResult<Rc<Instruction>> {
        self.scope.enter(Tag::Named("block"));
        let instructions = statements
            .iter()
            .map(|statement| self.translate_statement(context, statement))
            .collect::<Result<Rc<_>, _>>()?;
        self.scope.leave(Tag::Named("block"))?;
        Ok(Instruction::Batch(instructions).into())
    }

    fn translate_binary_expression(
        &mut self,
        operator: Binary,
        expression_type: Rc<Type>,
        left: &Rc<Expression>,
        right: &Rc<Expression>,
    ) -> CompileResult<(Rc<Instruction>, Rc<Value>)> {
        let operator = match operator {
            Binary::Add => IrBinaryOpcode::Add,
            Binary::Subtract => IrBinaryOpcode::Subtract,
            Binary::Multiply => IrBinaryOpcode::Multiply,
            Binary::Divide => IrBinaryOpcode::DivideSigned,
            Binary::LogicalAnd | Binary::BitAnd => IrBinaryOpcode::And,
            Binary::LogicalOr | Binary::BitOr => IrBinaryOpcode::Or,
            Binary::LeftShift => IrBinaryOpcode::LeftShift,
            Binary::RightShift => IrBinaryOpcode::ArithmeticRightShift,
            Binary::Equal => IrBinaryOpcode::Compare(Comparator::Equal),
            Binary::NotEqual => IrBinaryOpcode::Compare(Comparator::NotEqual),
            Binary::LessThan => IrBinaryOpcode::Compare(Comparator::SignedLessThan),
            Binary::LessThanEqual => IrBinaryOpcode::Compare(Comparator::SignedLessThanEqual),
            Binary::GreaterThan => IrBinaryOpcode::Compare(Comparator::SignedGreaterThan),
            Binary::GreaterThanEqual => IrBinaryOpcode::Compare(Comparator::SignedGreaterThanEqual),
            Binary::Assign => return self.translate_assignment(expression_type, left, right),
            Binary::As => {
                sys_error!("should be a cast expression rather than a binary expression")
            }
        };

        let (left_instruction, left) = self.translate_expression(left)?;
        let (right_instruction, right) = self.translate_expression(right)?;
        let result = self.declare(DeclType::Anonymous)?;

        let data_type = IrType::from(expression_type).into();

        let operation_instruction = Instruction::Binary {
            operator,
            data_type,
            result: result.clone(),
            left,
            right,
        };
        let instructions = [left_instruction, right_instruction, operation_instruction.into()];
        Ok((Instruction::Batch(instructions.into()).into(), result))
    }

    fn translate_lvalue(&mut self, lvalue: &Rc<LValue>) -> CompileResult<(Rc<Instruction>, Rc<Value>, Rc<IrType>)> {
        match lvalue.as_ref() {
            LValue::Id(identifier) => {
                let IrReference::Binding((value, ir_type)) = self.scope.lookup(identifier)?.unwrap() else {
                    sys_error!("should be inspected in type checking.");
                };
                Ok((Instruction::NoOperation.into(), value, ir_type))
            }
            LValue::Access(object, field) => {
                let (object_instruction, object_pointer, object_type) = self.translate_lvalue(object)?;
                let (index, field_type) = find_field(&object_type, field);
                let result = self.declare(DeclType::AnonymousLocal)?;
                let instruction = Instruction::GetElementPtr {
                    from: object_pointer,
                    to: result.clone(),
                    data_type: object_type,
                    index,
                };
                let instructions = [object_instruction, instruction.into()];
                Ok((Instruction::Batch(instructions.into()).into(), result, field_type))
            }
        }
    }

    fn translate_lvalue_expression(
        &mut self,
        lvalue_expression: Rc<Expression>,
    ) -> CompileResult<(Rc<Instruction>, Rc<Value>)> {
        let lvalue = LValue::try_from(lvalue_expression)?.into();
        let (instruction, value, _) = self.translate_lvalue(&lvalue)?;
        Ok((instruction, value))
    }

    fn translate_assignment(
        &mut self,
        lvalue_type: Rc<Type>,
        lvalue_expression: &Rc<Expression>,
        expression: &Rc<Expression>,
    ) -> CompileResult<(Rc<Instruction>, Rc<Value>)> {
        let (lvalue_instruction, lvalue) = self.translate_lvalue_expression(lvalue_expression.clone())?;
        let (expression_instruction, expression) = self.translate_expression(expression)?;

        let assign_instruction = Instruction::Store {
            data_type: IrType::from(lvalue_type).into(),
            from: expression,
            to: lvalue,
        };
        let instructions = [lvalue_instruction, expression_instruction, assign_instruction.into()];
        Ok((Instruction::Batch(instructions.into()).into(), Value::Void.into()))
    }

    fn translate_unary_expression(
        &mut self,
        operator: Unary,
        sub_type: Rc<Type>,
        expression: &Rc<Expression>,
    ) -> CompileResult<(Rc<Instruction>, Rc<Value>)> {
        let (expression_instruction, expression_value) = self.translate_expression(expression)?;

        let result = self.declare(DeclType::Anonymous)?;
        let data_type = Rc::new(IrType::from(sub_type));
        let unary_instruction = match operator {
            Unary::LogicalNot | Unary::BitNot => {
                let mask = match data_type.as_ref() {
                    IrType::Bool => 1,
                    IrType::Int(_) => -1i32,
                    _ => sys_error!("type check messed."),
                };
                Instruction::Binary {
                    operator: IrBinaryOpcode::Xor,
                    data_type,
                    result: result.clone(),
                    left: Value::ImmediateI32(mask).into(),
                    right: expression_value,
                }
            }
            Unary::Negative => Instruction::Binary {
                operator: IrBinaryOpcode::Subtract,
                data_type,
                result: result.clone(),
                left: Value::ImmediateI32(0).into(),
                right: expression_value,
            },
        };
        let unary_instruction = unary_instruction.into();

        Ok((
            Instruction::Batch([expression_instruction, unary_instruction].into()).into(),
            result,
        ))
    }

    fn translate_call_expression(
        &mut self,
        function_name: &Rc<String>,
        arguments: &Array<Expression>,
    ) -> CompileResult<(Rc<Instruction>, Rc<Value>)> {
        let receiver = self.declare(DeclType::Anonymous)?;
        let Some(IrReference::Binding((function_id, function_type))) = self.scope.lookup(function_name)? else {
            sys_error!("undeclared identifier.");
        };
        let IrType::Function {
            return_type,
            parameter_types: _,
        } = function_type.as_ref()
        else {
            sys_error!("need a function type here.");
        };
        let (mut instructions, arguments) = arguments
            .iter()
            .map(|argument| self.translate_expression(argument))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .unzip::<_, _, Vec<_>, Vec<_>>();
        let arguments = arguments.into();
        let call_instruction = Instruction::Call {
            receiver: match return_type.as_ref() {
                IrType::Void => None,
                _ => Some(receiver.clone()),
            },
            function: IrFunctionPrototype {
                function_type,
                id: function_id,
            },
            arguments,
        };
        instructions.push(call_instruction.into());
        Ok((Instruction::Batch(instructions.into()).into(), receiver))
    }

    fn translate_cast_expression(
        &mut self,
        expression: &Rc<Expression>,
        from_type: &Rc<Type>,
        to_type: &Rc<Type>,
    ) -> CompileResult<(Rc<Instruction>, Rc<Value>)> {
        let (instruction, from_value) = self.translate_expression(expression)?;
        match (from_type.as_ref(), to_type.as_ref()) {
            (Type::Int(from_width), Type::Int(to_width)) => match from_width.cmp(to_width) {
                Ordering::Equal => Ok((instruction, from_value)),
                Ordering::Less => {
                    let to_value = self.declare(DeclType::Anonymous)?;
                    let from_type: IrType = from_type.clone().into();
                    let to_type: IrType = to_type.clone().into();
                    let instruction = Instruction::Batch(
                        [
                            instruction,
                            Instruction::SignExtension {
                                from: (from_value, from_type.into()),
                                to: (to_value.clone(), to_type.into()),
                            }
                            .into(),
                        ]
                        .into(),
                    );
                    Ok((instruction.into(), to_value))
                }
                Ordering::Greater => {
                    let to_value = self.declare(DeclType::Anonymous)?;
                    let from_type: IrType = from_type.clone().into();
                    let to_type: IrType = to_type.clone().into();
                    let instruction = Instruction::Batch(
                        [
                            instruction,
                            Instruction::Truncate {
                                from: (from_value, from_type.into()),
                                to: (to_value.clone(), to_type.into()),
                            }
                            .into(),
                        ]
                        .into(),
                    );
                    Ok((instruction.into(), to_value))
                }
            },
            (_, _) => sys_error!("should be inspected before."),
        }
    }

    fn translate_expression(&mut self, expression: &Rc<Expression>) -> CompileResult<(Rc<Instruction>, Rc<Value>)> {
        match expression.as_ref() {
            Expression::Identifier(identifier) => {
                let Some(IrReference::Binding((value, data_type))) = self.scope.lookup(identifier)? else {
                    sys_error!("should be inspected in type checking.");
                };
                match value.as_ref() {
                    Value::Register(_) | Value::Parameter(_) => Ok((Instruction::NoOperation.into(), value)),
                    Value::Pointer(_) => {
                        let result = self.declare(DeclType::Anonymous)?;
                        let instruction = Instruction::Load {
                            data_type,
                            from: value,
                            to: result.clone(),
                        };
                        Ok((instruction.into(), result))
                    }
                    _ => panic!(),
                }
            }
            Expression::IntLiteral(literal) => {
                Ok((Instruction::NoOperation.into(), Value::ImmediateI32(*literal).into()))
            }
            Expression::CharLiteral(literal) => Ok((
                Instruction::NoOperation.into(),
                Value::ImmediateI8(*literal as i8).into(),
            )),
            Expression::FloatLiteral(_) => todo!(),
            Expression::BoolLiteral(literal) => {
                Ok((Instruction::NoOperation.into(), Value::ImmediateBool(*literal).into()))
            }
            Expression::Unary(operator, sub_type, expression) => {
                self.translate_unary_expression(*operator, sub_type.clone(), expression)
            }
            Expression::Binary(operator, child_type, left, right) => {
                self.translate_binary_expression(*operator, child_type.clone(), left, right)
            }
            Expression::Call(function_name, arguments) => self.translate_call_expression(function_name, arguments),
            Expression::Cast(expression, from_type, to_type) => {
                self.translate_cast_expression(expression, from_type, to_type)
            }
            Expression::StructSpawn(name, fields) => {
                let (field_instructions, field_values) = fields
                    .iter()
                    .map(Rc::as_ref)
                    .map(|FieldInit(_, expression)| expression)
                    .map(|expression| self.translate_expression(expression))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .unzip::<_, _, Vec<_>, Vec<_>>();
                let field_instructions: Array<_> = field_instructions.into();
                let field_values: Array<_> = field_values.into();
                let IrReference::StructDef(id, _, fields) = self.scope.lookup(name)?.unwrap() else {
                    sys_error!("should be inspected in type checking.");
                };
                let ir_type = IrType::Struct {
                    id,
                    name: name.clone(),
                    fields: fields.clone(),
                };
                let ir_type = Rc::new(ir_type);
                let pointer = self.declare(DeclType::Local(format!("sp.{name}").into(), ir_type.clone()))?;
                let fields = field_values
                    .iter()
                    .zip(fields.iter().map(Rc::as_ref).map(|IrField(_, field_type)| field_type))
                    .map(|(value, ty)| (value.clone(), ty.clone()))
                    .map(Rc::new)
                    .collect::<Array<_>>();
                let result = self.declare(DeclType::Anonymous)?;
                let instructions = [
                    Instruction::Batch(field_instructions),
                    Instruction::Allocate((pointer.clone(), ir_type.clone())),
                    Instruction::Store {
                        data_type: ir_type.clone(),
                        from: Value::ImmediateStruct(fields).into(),
                        to: pointer.clone(),
                    },
                    Instruction::Load {
                        data_type: ir_type.clone(),
                        from: pointer.clone(),
                        to: result.clone(),
                    },
                ];
                let instructions = instructions.into_iter().map(Rc::new).collect();
                Ok((Instruction::Batch(instructions).into(), result))
            }
            Expression::Access(expression, object_type, field) => {
                let object_type = Rc::new(IrType::from(object_type.as_ref()));
                let (object_instruction, object_value) = self.translate_expression(expression)?;
                let (index, _) = find_field(&object_type, field);
                let result = self.declare(DeclType::Anonymous)?;
                let extract_value = Instruction::ExtractValue {
                    from: object_value,
                    to: result.clone(),
                    data_type: object_type,
                    index,
                };
                let instructions = [object_instruction, extract_value.into()];
                Ok((Instruction::Batch(instructions.into()).into(), result))
            }
        }
    }

    fn translate_expression_statement(&mut self, expression: &Rc<Expression>) -> CompileResult<Rc<Instruction>> {
        let (instruction, _) = self.translate_expression(expression)?;
        Ok(instruction)
    }

    /// source code example:
    /// ```plain_text
    /// if condition {
    ///     do_something_a
    /// } else {
    ///     do_something_b
    /// }
    /// ```
    ///
    ///
    /// translation:
    /// ```llvm
    ///     br label %if_start_label
    /// if_start_label:
    ///     condition_instructions
    ///     br i1 condition_id, label %when_true_label, label %when_false_label
    /// when_true_label:
    ///     do_something_a_instructions
    ///     br label %if_end_label
    /// when_false_label:
    ///     do_something_b_instructions
    ///     br label %if_end_label
    /// if_end_label:
    /// ```
    ///
    fn translate_if_statement(&mut self, context: &Context, if_detail: &IfDetail) -> CompileResult<Rc<Instruction>> {
        let IfDetail {
            condition,
            true_body,
            false_body,
        } = if_detail;
        let if_start = self.declare(DeclType::Label("if_start"))?;
        let when_true = self.declare(DeclType::Label("when_true"))?;
        let when_false = self.declare(DeclType::Label("when_false"))?;
        let if_end = self.declare(DeclType::Label("if_end"))?;

        let jump_to_start = Instruction::Jump(if_start.clone());
        let start_label = Instruction::Label(if_start);
        let (condition_instructions, condition) = self.translate_expression(condition)?;
        let conditional_jump = Instruction::JumpIf {
            condition,
            when_true: when_true.clone(),
            when_false: when_false.clone(),
        };
        let when_true_label = Instruction::Label(when_true);
        let body_instructions = self.translate_statement(context, true_body)?;
        let jump_true_to_end = Instruction::Jump(if_end.clone());
        let when_false_label = Instruction::Label(when_false);
        let false_body_instructions = match false_body {
            Some(statement) => self.translate_statement(context, statement)?,
            None => Instruction::NoOperation.into(),
        };
        let jump_false_to_end = Instruction::Jump(if_end.clone());
        let end_label = Instruction::Label(if_end);

        let instructions = [
            jump_to_start.into(),
            start_label.into(),
            condition_instructions,
            conditional_jump.into(),
            when_true_label.into(),
            body_instructions,
            jump_true_to_end.into(),
            when_false_label.into(),
            false_body_instructions,
            jump_false_to_end.into(),
            end_label.into(),
        ];
        Ok(Instruction::Batch(instructions.into()).into())
    }

    /// source code example:
    /// ```llvm
    /// while condition {
    ///     do_something
    /// }
    /// ```
    ///
    ///
    /// translation:
    /// ```llvm
    ///     br label %check_point_label
    /// check_point_label:
    ///     condition_instructions
    ///     br i1 condition_id, label %loop_label, label %loop_end_label
    /// loop_start_label:
    ///     do_something_instructions
    ///     br label %check_point_label
    /// loop_end_label:
    /// ```
    ///
    fn translate_while_statement(
        &mut self,
        context: &Context,
        WhileDetail(condition, body): &WhileDetail,
    ) -> CompileResult<Rc<Instruction>> {
        let check_point = self.declare(DeclType::Label("check_point"))?;
        let loop_start = self.declare(DeclType::Label("loop_start"))?;
        let loop_end = self.declare(DeclType::Label("loop_end"))?;

        let jump_to_check_point = Instruction::Jump(check_point.clone());
        let check_point_label = Instruction::Label(check_point.clone());
        let (condition_instructions, condition) = self.translate_expression(condition)?;
        let conditional_jump = Instruction::JumpIf {
            condition,
            when_true: loop_start.clone(),
            when_false: loop_end.clone(),
        };
        let loop_start_label = Instruction::Label(loop_start);
        let body_instructions = self.translate_statement(context, body)?;
        let jump_back_to_check_point = Instruction::Jump(check_point);
        let loop_end_label = Instruction::Label(loop_end);

        let instructions = [
            jump_to_check_point.into(),
            check_point_label.into(),
            condition_instructions,
            conditional_jump.into(),
            loop_start_label.into(),
            body_instructions,
            jump_back_to_check_point.into(),
            loop_end_label.into(),
        ];
        Ok(Instruction::Batch(instructions.into()).into())
    }

    fn translate_let_statement(
        &mut self,
        LetDetail(Variable(identifier, var_type, _), expression): &LetDetail,
    ) -> CompileResult<Rc<Instruction>> {
        let data_type = IrType::from(var_type.as_ref()).into();
        let (expression_instructions, expression) = self.translate_expression(expression)?;
        let assign_instruction = if self.scope.is_global()? {
            let Some(IrReference::Binding((lvalue, _))) = self.scope.lookup(identifier)? else {
                sys_error!("must find it.");
            };
            Instruction::Global {
                lvalue,
                data_type,
                value: expression,
            }
        } else {
            let lvalue = self.declare(DeclType::Local(identifier.clone(), data_type.clone()))?;
            Instruction::Batch(
                [
                    Instruction::Allocate((lvalue.clone(), data_type.clone())).into(),
                    Instruction::Store {
                        data_type,
                        from: expression,
                        to: lvalue,
                    }
                    .into(),
                ]
                .into(),
            )
        };
        let instructions = [expression_instructions, assign_instruction.into()].into();
        Ok(Instruction::Batch(instructions).into())
    }

    fn translate_return_statement(&mut self, return_value: Option<Rc<Expression>>) -> CompileResult<Rc<Instruction>> {
        let Some(return_value) = return_value else {
            return Ok(Instruction::ReturnVoid.into());
        };
        let (expression_instructions, expression) = self.translate_expression(&return_value)?;
        let function_name = self.scope.current_function()?;
        let Some(IrReference::Binding((_, function_type))) = self.scope.lookup(&function_name)? else {
            sys_error!("must be a function type");
        };
        let IrType::Function {
            return_type,
            parameter_types: _,
        } = function_type.as_ref()
        else {
            sys_error!("must be a function type");
        };
        let return_type = return_type.clone();
        let instructions = [
            expression_instructions,
            Instruction::Return {
                return_type,
                value: expression,
            }
            .into(),
        ];
        Ok(Instruction::Batch(instructions.into()).into())
    }

    fn translate_define_statement(
        &mut self,
        context: &Context,
        DefineDetail {
            prototype,
            builtin,
            body,
        }: &DefineDetail,
    ) -> CompileResult<Rc<Instruction>> {
        let FunctionPrototype {
            identifier,
            parameters,
            function_type: _,
        } = prototype.as_ref();

        let Some(IrReference::Binding((function, function_type))) = self.scope.lookup(identifier)? else {
            sys_error!("must pre scanned it.");
        };
        if *builtin {
            let Some(ir_code) = get_builtin(&self.document_path, identifier, &function) else {
                return Err(CompileError::BuiltinFunctionFileNotExist(identifier.clone()));
            };
            return Ok(Instruction::BuiltinDefinition(ir_code).into());
        }
        let IrType::Function {
            return_type,
            parameter_types,
        } = function_type.as_ref()
        else {
            sys_error!("must be a function type.");
        };
        self.scope.enter(Tag::Function(identifier.clone()));
        let parameters = parameters
            .iter()
            .map(Rc::as_ref)
            .zip(parameter_types.iter())
            .map(|(Parameter(identifier), parameter_type)| {
                self.declare(DeclType::Param(identifier.clone(), parameter_type.clone()))
            })
            .collect::<Result<Rc<_>, _>>()?;
        let body_instructions = [
            self.translate_statement(context, body)?,
            if let IrType::Void = return_type.as_ref() {
                Instruction::ReturnVoid.into()
            } else {
                Instruction::Unreachable.into()
            },
        ];
        let body = Instruction::Batch(body_instructions.into()).into();
        self.scope.leave(Tag::Function(identifier.clone()))?;

        let definition = Instruction::Definition(
            IrFunctionPrototype {
                function_type,
                id: function,
            },
            parameters,
            body,
        );
        Ok(definition.into())
    }

    fn translate_statement(&mut self, context: &Context, statement: &Statement) -> CompileResult<Rc<Instruction>> {
        match statement {
            Statement::Empty => Ok(Instruction::NoOperation.into()),
            Statement::Block(statements) => self.translate_block_statement(context, statements),
            Statement::Return(return_value) => self.translate_return_statement(return_value.clone()),
            Statement::Expression(expression) => self.translate_expression_statement(expression),
            Statement::If(if_detail) => self.translate_if_statement(context, if_detail),
            Statement::While(while_detail) => self.translate_while_statement(context, while_detail),
            Statement::Let(let_detail) => self.translate_let_statement(let_detail),
            Statement::Define(define_detail) => self.translate_define_statement(context, define_detail),
            Statement::Using(UsingPath(document_path, symbol)) => {
                let id = context.id_map.get(document_path).unwrap();
                let Some(referent) = context.ir_model_map.get(id).unwrap().get(symbol) else {
                    sys_error!("should be inspected: used symbol must exist");
                };
                match referent.as_ref() {
                    IrReference::Binding((value, ir_type)) => {
                        self.scope
                            .declare(symbol.clone(), IrReference::Binding((value.clone(), ir_type.clone())))?;
                    }
                    IrReference::StructDef(id, name, fields) => {
                        self.scope.declare(
                            symbol.clone(),
                            IrReference::StructDef(*id, name.clone(), fields.clone()),
                        )?;
                    }
                }
                Ok(Instruction::NoOperation.into())
            }
            Statement::Struct(StructDetail { id, name, fields }) => {
                let field_types = fields
                    .iter()
                    .map(Rc::as_ref)
                    .map(|Field(_, field_type)| field_type)
                    .map(Rc::as_ref)
                    .map(IrType::from)
                    .map(Rc::new)
                    .collect::<Array<_>>();
                let ir_name = format!("s{id}.{name}");
                Ok(Instruction::Struct {
                    name: ir_name.into(),
                    fields: field_types,
                }
                .into())
            }
        }
    }

    fn translate_document(&mut self, context: &Context, document: &Document) -> CompileResult<Rc<Instruction>> {
        let instructions = document
            .statements
            .iter()
            .map(|statement| self.translate_statement(context, statement))
            .collect::<Result<Rc<_>, _>>()?;
        Ok(Instruction::Batch(instructions).into())
    }

    /// # Errors
    /// # Panics
    pub fn pre_scan_global(&mut self, context: &mut Context, document_id: DocumentId) -> CompileResult<()> {
        self.scope.enter(Tag::Global);
        let document = context.document_map.get(&document_id).unwrap();
        let mut ir_model_map = HashMap::new();
        document
            .statements
            .iter()
            .try_for_each::<_, CompileResult<_>>(|statement| match statement.as_ref() {
                Statement::Empty
                | Statement::Block(_)
                | Statement::Return(_)
                | Statement::Expression(_)
                | Statement::If(_)
                | Statement::While(_) => sys_error!("unchecked global procedures"),
                Statement::Let(LetDetail(Variable(identifier, var_type, _), _)) => {
                    let data_type = Rc::new(IrType::from(var_type.clone()));
                    let value = self.declare(DeclType::Global(identifier.clone(), data_type.clone()))?;
                    ir_model_map.insert(identifier.clone(), IrReference::Binding((value, data_type)).into());
                    Ok(())
                }
                Statement::Define(DefineDetail {
                    prototype,
                    builtin: _,
                    body: _,
                }) => {
                    let FunctionPrototype {
                        identifier,
                        parameters: _,
                        function_type,
                    } = prototype.as_ref();

                    let function_type = Rc::new(IrType::from(function_type.clone()));
                    let value = self.declare(DeclType::Function(identifier.clone(), function_type.clone()))?;
                    ir_model_map.insert(identifier.clone(), IrReference::Binding((value, function_type)).into());
                    Ok(())
                }
                Statement::Using(_) => Ok(()),
                Statement::Struct(StructDetail { id: _, name, fields }) => {
                    let Reference::StructDef(id, _, _) = context
                        .reference_map
                        .get(&document_id)
                        .unwrap()
                        .get(name)
                        .unwrap()
                        .as_ref()
                    else {
                        sys_error!("must be a struct definition");
                    };
                    let fields = fields
                        .iter()
                        .map(Rc::as_ref)
                        .map(|Field(field_name, field_type)| {
                            let field_type = IrType::from(field_type.as_ref());
                            IrField(field_name.clone(), field_type.into())
                        })
                        .map(Rc::new)
                        .collect::<Array<_>>();
                    self.scope
                        .declare(name.clone(), IrReference::StructDef(*id, name.clone(), fields.clone()))?;
                    ir_model_map.insert(
                        name.clone(),
                        IrReference::StructDef(*id, name.clone(), fields.clone()).into(),
                    );
                    Ok(())
                }
            })?;
        context.ir_model_map.insert(document_id, ir_model_map);
        Ok(())
    }

    /// # Errors
    pub fn translate(&mut self, context: &mut Context, document_id: DocumentId) -> CompileResult<()> {
        let Some(document) = context.document_map.get(&document_id) else {
            sys_error!("document must exist");
        };
        let instruction = self.translate_document(context, document)?;
        context.instruction.insert(document_id, instruction);
        self.scope.leave(Tag::Global)?;
        Ok(())
    }
}
