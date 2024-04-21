use crate::{
    compiler::{
        err::CompileError,
        scope::Tag,
        syntax::ast::{
            crumb::{FunctionPrototype, Mutability, Parameter, Variable},
            expression::Expression,
            operator::{Binary, Unary},
            statement::{DefineDetail, IfDetail, LetDetail, Statement},
            ty::Type,
            Program,
        },
    },
    system_error,
};

use super::err::IrError;
use super::{
    builtin::{deinit_builtin_scope, init_builtin_scope},
    id::{
        next_anonymous_id, next_function_id, next_global_id, next_label_id, next_parameter_id,
        next_variable_id,
    },
    instruction::{
        ir_type::IrType, Comparator, Instruction, IrBinaryOpcode, IrFunctionPrototype, IrScope,
        Value,
    },
};

pub struct Translator {
    scope: IrScope,
}

impl Translator {
    #[must_use]
    pub fn new() -> Translator {
        Translator {
            scope: IrScope::new(),
        }
    }

    fn declare_anonymous(&self) -> Value {
        let id = next_anonymous_id();
        Value::Register(format!("t{id}"))
    }

    fn declare_parameter(
        &mut self,
        symbol: String,
        data_type: IrType,
    ) -> Result<Value, CompileError> {
        let id = next_parameter_id();
        let value = Value::Parameter(format!("p{id}.{symbol}"));
        self.scope.declare(symbol, (value.clone(), data_type))?;
        Ok(value)
    }

    fn declare_immutable(
        &mut self,
        symbol: String,
        data_type: IrType,
    ) -> Result<Value, CompileError> {
        let id = next_variable_id();
        let value = Value::Register(format!("v{id}.{symbol}"));
        self.scope.overwrite(symbol, (value.clone(), data_type))?;
        Ok(value)
    }

    fn declare_mutable(
        &mut self,
        symbol: String,
        data_type: IrType,
    ) -> Result<Value, CompileError> {
        let id = next_variable_id();
        let value = Value::StackPointer(format!("v{id}.{symbol}"));
        self.scope.overwrite(symbol, (value.clone(), data_type))?;
        Ok(value)
    }

    fn declare_global(&mut self, symbol: String, data_type: IrType) -> Result<Value, CompileError> {
        let id = next_global_id();
        let value = Value::GlobalPointer(format!("g{id}.{symbol}"));
        self.scope.declare(symbol, (value.clone(), data_type))?;
        Ok(value)
    }

    fn declare_constant(
        &mut self,
        symbol: String,
        data_type: IrType,
    ) -> Result<Value, CompileError> {
        let id = next_global_id();
        let value = Value::GlobalPointer(format!("c{id}.{symbol}"));
        self.scope.declare(symbol, (value.clone(), data_type))?;
        Ok(value)
    }

    fn declare_function(
        &mut self,
        symbol: String,
        data_type: IrType,
    ) -> Result<Value, CompileError> {
        let id = next_function_id();
        let value = Value::Function(format!("f{id}.{symbol}"));
        self.scope.declare(symbol, (value.clone(), data_type))?;
        Ok(value)
    }

    fn declare_label(&self) -> Value {
        let id = next_label_id();
        Value::Label(format!("l{id}"))
    }

    fn translate_block_statement(
        &mut self,
        statements: &[Statement],
    ) -> Result<Instruction, CompileError> {
        self.scope.enter(Tag::Named("block"));
        let instructions = statements
            .iter()
            .map(|statement| self.translate_statement(statement))
            .collect::<Result<Vec<_>, _>>()?;
        self.scope.leave(Tag::Named("block"))?;
        Ok(Instruction::Batch(instructions))
    }

    fn translate_binary_expression(
        &mut self,
        operator: &Binary,
        sub_type: &Type,
        left: &Expression,
        right: &Expression,
    ) -> Result<(Instruction, Value), CompileError> {
        let operator = match operator {
            Binary::Add => IrBinaryOpcode::Add,
            Binary::Subtract => IrBinaryOpcode::Subtract,
            Binary::Multiply => IrBinaryOpcode::Multiply,
            Binary::Divide => IrBinaryOpcode::DivideSigned,
            Binary::LogicalAnd | Binary::BitAnd => IrBinaryOpcode::And,
            Binary::LogicalOr | Binary::BitOr => IrBinaryOpcode::Or,
            Binary::Equal => IrBinaryOpcode::Compare(Comparator::Equal),
            Binary::NotEqual => IrBinaryOpcode::Compare(Comparator::NotEqual),
            Binary::LessThan => IrBinaryOpcode::Compare(Comparator::SignedLessThan),
            Binary::LessThanEqual => IrBinaryOpcode::Compare(Comparator::SignedLessThanEqual),
            Binary::GreaterThan => IrBinaryOpcode::Compare(Comparator::SignedGreaterThan),
            Binary::GreaterThanEqual => IrBinaryOpcode::Compare(Comparator::SignedGreaterThanEqual),
            Binary::Assign => return self.translate_assignment(sub_type, left, right),
        };

        let (left_instruction, left) = self.translate_expression(left)?;
        let (right_instruction, right) = self.translate_expression(right)?;
        let result = self.declare_anonymous();

        let data_type = IrType::from(sub_type);

        Ok((
            Instruction::Batch(vec![
                left_instruction,
                right_instruction,
                Instruction::Binary {
                    operator,
                    data_type,
                    result: result.clone(),
                    left,
                    right,
                },
            ]),
            result,
        ))
    }

    fn translate_lvalue_expression(
        &mut self,
        lvalue_expression: &Expression,
    ) -> Result<Value, CompileError> {
        let Expression::Identifier(identifier) = lvalue_expression else {
            return Err(system_error!("should be inspected in type check."));
        };
        let Some((value, _)) = self.scope.lookup(identifier)? else {
            return Err(system_error!("undeclared identifier here."));
        };
        match value {
            Value::StackPointer(_) | Value::GlobalPointer(_) | Value::Parameter(_) => Ok(value),
            _ => Err(system_error!("should be inspected in type check.")),
        }
    }

    fn translate_assignment(
        &mut self,
        sub_type: &Type,
        lvalue: &Expression,
        expression: &Expression,
    ) -> Result<(Instruction, Value), CompileError> {
        let lvalue = self.translate_lvalue_expression(lvalue)?;
        let (expression_instruction, expression) = self.translate_expression(expression)?;
        let assign_instruction = Instruction::Store {
            data_type: IrType::from(sub_type),
            from: expression,
            to: lvalue,
        };
        Ok((
            Instruction::Batch(vec![expression_instruction, assign_instruction]),
            Value::Void,
        ))
    }

    fn translate_unary_expression(
        &mut self,
        operator: Unary,
        sub_type: &Type,
        expression: &Expression,
    ) -> Result<(Instruction, Value), CompileError> {
        let (expression_instruction, expression_value) = self.translate_expression(expression)?;

        let result = self.declare_anonymous();
        let data_type = IrType::from(sub_type);
        let unary_instruction = match operator {
            Unary::LogicalNot | Unary::BitNot => {
                let mask = match data_type {
                    IrType::Bool => 1,
                    IrType::I32 => -1i32,
                    _ => return Err(system_error!("type check messed.")),
                };
                Instruction::Binary {
                    operator: IrBinaryOpcode::Xor,
                    data_type,
                    result: result.clone(),
                    left: Value::ImmediateI32(mask),
                    right: expression_value,
                }
            }
            Unary::Negative => Instruction::Binary {
                operator: IrBinaryOpcode::Subtract,
                data_type,
                result: result.clone(),
                left: Value::ImmediateI32(0),
                right: expression_value,
            },
        };

        Ok((
            Instruction::Batch(vec![expression_instruction, unary_instruction]),
            result,
        ))
    }

    fn translate_expression(
        &mut self,
        expression: &Expression,
    ) -> Result<(Instruction, Value), CompileError> {
        match expression {
            Expression::Identifier(identifier) => self.scope.lookup(identifier)?.map_or(
                Err(system_error!("should be inspected in type checking.")),
                |(value, data_type)| match value {
                    Value::Register(_) | Value::Parameter(_) => {
                        Ok((Instruction::NoOperation, value))
                    }
                    Value::StackPointer(_) | Value::GlobalPointer(_) => {
                        let result = self.declare_anonymous();
                        let instruction = Instruction::Load {
                            data_type,
                            from: value,
                            to: result.clone(),
                        };
                        Ok((instruction, result))
                    }
                    _ => panic!(),
                },
            ),
            Expression::IntLiteral(literal) => {
                Ok((Instruction::NoOperation, Value::ImmediateI32(*literal)))
            }
            Expression::FloatLiteral(_) => todo!(),
            Expression::BoolLiteral(literal) => {
                Ok((Instruction::NoOperation, Value::ImmediateBool(*literal)))
            }
            Expression::Unary(operator, sub_type, expression) => {
                self.translate_unary_expression(*operator, sub_type, expression)
            }
            Expression::Binary(operator, expression_type, left, right) => {
                self.translate_binary_expression(operator, expression_type, left, right)
            }
            Expression::Call(function_name, arguments) => {
                let receiver = self.declare_anonymous();
                let Some((function_id, function_type)) = self.scope.lookup(function_name)? else {
                    return Err(system_error!("undeclared identifier."));
                };
                let IrType::Function {
                    return_type,
                    parameter_types: _,
                } = function_type.clone()
                else {
                    return Err(system_error!("need a function type here."));
                };
                let (mut instructions, arguments) = arguments
                    .iter()
                    .map(|argument| self.translate_expression(argument))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .unzip::<_, _, Vec<_>, Vec<_>>();
                let call_instruction = Instruction::Call {
                    receiver: if let IrType::Void = *return_type {
                        None
                    } else {
                        Some(receiver.clone())
                    },
                    function: IrFunctionPrototype {
                        function_type,
                        id: function_id,
                    },
                    arguments,
                };
                instructions.push(call_instruction);
                Ok((Instruction::Batch(instructions), receiver))
            }
        }
    }

    fn translate_expression_statement(
        &mut self,
        expression: &Expression,
    ) -> Result<Instruction, CompileError> {
        let (instruction, _) = self.translate_expression(expression)?;
        Ok(instruction)
    }

    /// source code example:
    /// ```
    /// if condition {
    ///     do_something_a();
    /// } else {
    ///     do_something_b();
    /// }
    /// ```
    ///
    ///
    /// translation:
    /// ```
    ///     br label %start_label
    /// start_label:
    ///     condition_instructions
    ///     br i1 condition_id, label %when_true_label, label %when_false_label
    /// when_true_label:
    ///     do_something_a();
    ///     br label %end_label
    /// when_false_label:
    ///     do_something_b();
    ///     br label %end_label
    /// end_label:
    /// ```
    fn translate_if_statement(
        &mut self,
        if_detail: &IfDetail,
    ) -> Result<Instruction, CompileError> {
        let IfDetail {
            condition,
            true_body,
            false_body,
        } = if_detail;
        let start = self.declare_label();
        let when_true = self.declare_label();
        let when_false = self.declare_label();
        let end = self.declare_label();

        let jump_to_start = Instruction::Jump(start.clone());
        let start_label = Instruction::Label(start);
        let (condition_instructions, condition) = self.translate_expression(condition)?;
        let conditional_jump = Instruction::JumpIf {
            condition,
            when_true: when_true.clone(),
            when_false: when_false.clone(),
        };
        let when_true_label = Instruction::Label(when_true);
        let body_instructions = self.translate_statement(true_body)?;
        let jump_true_to_end = Instruction::Jump(end.clone());
        let when_false_label = Instruction::Label(when_false);
        let false_body_instructions = match false_body {
            Some(statement) => self.translate_statement(statement)?,
            None => Instruction::NoOperation,
        };
        let jump_false_to_end = Instruction::Jump(end.clone());
        let end_label = Instruction::Label(end);

        Ok(Instruction::Batch(vec![
            jump_to_start,
            start_label,
            condition_instructions,
            conditional_jump,
            when_true_label,
            body_instructions,
            jump_true_to_end,
            when_false_label,
            false_body_instructions,
            jump_false_to_end,
            end_label,
        ]))
    }

    fn translate_let_statement(
        &mut self,
        let_detail: &LetDetail,
    ) -> Result<Instruction, CompileError> {
        let LetDetail(Variable(identifier, var_type, mutability), expression) = let_detail;
        let data_type = IrType::from(var_type);
        let (expression_instructions, expression) = self.translate_expression(expression)?;
        let assign_instruction = match (self.scope.is_global()?, mutability) {
            (true, Mutability::Mutable) => {
                let lvalue = self.declare_global(identifier.clone(), data_type.clone())?;
                Instruction::Global {
                    lvalue,
                    data_type,
                    value: expression,
                }
            }
            (true, Mutability::Immutable) => {
                let lvalue = self.declare_constant(identifier.clone(), data_type.clone())?;
                Instruction::Constant {
                    lvalue,
                    data_type,
                    value: expression,
                }
            }
            (false, Mutability::Mutable) => {
                let lvalue = self.declare_mutable(identifier.clone(), data_type.clone())?;
                Instruction::Batch(vec![
                    Instruction::Allocate(lvalue.clone(), data_type.clone()),
                    Instruction::Store {
                        data_type,
                        from: expression,
                        to: lvalue,
                    },
                ])
            }
            (false, Mutability::Immutable) => {
                let lvalue = self.declare_immutable(identifier.clone(), data_type.clone())?;
                Instruction::Copy {
                    data_type,
                    from: expression,
                    to: lvalue,
                }
            }
        };
        let instructions = vec![expression_instructions, assign_instruction];
        Ok(Instruction::Batch(instructions))
    }

    fn translate_return_statement(
        &mut self,
        return_value: Option<&Expression>,
    ) -> Result<Instruction, CompileError> {
        let Some(return_value) = return_value else {
            return Ok(Instruction::ReturnVoid);
        };
        let (expression_instructions, expression) = self.translate_expression(return_value)?;
        let function_name = self.scope.current_function()?;
        let Some((
            _,
            IrType::Function {
                return_type,
                parameter_types: _,
            },
        )) = self.scope.lookup(&function_name)?
        else {
            return Err(system_error!("must be a function type"));
        };
        let return_type = *return_type;
        let instructions = vec![
            expression_instructions,
            Instruction::Return {
                return_type,
                value: expression,
            },
        ];
        Ok(Instruction::Batch(instructions))
    }

    fn translate_define_statement(
        &mut self,
        DefineDetail {
            prototype:
                FunctionPrototype {
                    identifier,
                    parameters,
                    function_type,
                },
            body,
        }: &DefineDetail,
    ) -> Result<Instruction, CompileError> {
        let Type::Function {
            return_type,
            parameter_types,
        } = function_type
        else {
            return Err(system_error!(""));
        };
        let return_type = IrType::from(return_type.as_ref());
        let parameter_types = parameter_types.iter().map(IrType::from).collect();
        let function_type = IrType::Function {
            return_type: Box::new(return_type.clone()),
            parameter_types,
        };
        let function = self.declare_function(identifier.clone(), function_type.clone())?;

        self.scope.enter(Tag::Function(identifier.clone()));
        let parameters = parameters
            .iter()
            .map(|Parameter(identifier)| self.declare_parameter(identifier.clone(), IrType::I32))
            .collect::<Result<Vec<_>, _>>()?;
        let body = Instruction::Batch(vec![
            self.translate_statement(body)?,
            if return_type == IrType::Void {
                Instruction::ReturnVoid
            } else {
                Instruction::Unreachable
            },
        ]);
        let body = Box::new(body);
        self.scope.leave(Tag::Function(identifier.clone()))?;

        Ok(Instruction::Definition(
            IrFunctionPrototype {
                function_type,
                id: function,
            },
            parameters,
            body,
        ))
    }

    fn translate_statement(&mut self, statement: &Statement) -> Result<Instruction, CompileError> {
        match statement {
            Statement::Empty => Ok(Instruction::NoOperation),
            Statement::Block(statements) => self.translate_block_statement(statements),
            Statement::Return(return_value) => {
                self.translate_return_statement(return_value.as_ref())
            }
            Statement::Expression(expression) => self.translate_expression_statement(expression),
            Statement::If(if_detail) => self.translate_if_statement(if_detail),
            Statement::Let(let_detail) => self.translate_let_statement(let_detail),
            Statement::Define(define_detail) => self.translate_define_statement(define_detail),
        }
    }

    fn translate_program(&mut self, program: &Program) -> Result<Instruction, CompileError> {
        let instructions = program
            .statements
            .iter()
            .map(|statement| self.translate_statement(statement))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Instruction::Batch(instructions))
    }

    fn generate_main(&mut self) -> Result<String, CompileError> {
        let Some((main_id, _)) = self.scope.lookup(&String::from("main"))? else {
            return Err(IrError::UndefinedMain.into());
        };
        let template = format!(
            "\
define i32 @main() {{
    %main_return = call i32 {main_id}()
    ret i32 %main_return
}}\
            "
        );
        Ok(template)
    }

    /// # Errors
    pub fn translate(&mut self, program: &Program) -> Result<String, CompileError> {
        let mut llvm_code = String::new();
        init_builtin_scope(&mut self.scope, &mut llvm_code)?;
        self.scope.enter(Tag::Global);
        let user_program = self.translate_program(program)?;
        let user_program = user_program.to_string();
        llvm_code.push_str(&user_program);
        let main = self.generate_main()?;
        llvm_code.push_str(&main);
        self.scope.leave(Tag::Global)?;
        deinit_builtin_scope(&mut self.scope)?;
        Ok(llvm_code)
    }
}

impl Default for Translator {
    fn default() -> Self {
        Self::new()
    }
}
