use crate::compiler::{
    err::CompileError,
    scope::Tag,
    syntax::ast::{
        BinaryOperator, DefineDetail, Expression, FunctionPrototype, IfDetail, LetDetail,
        Parameter, Program, Statement, Type, UnaryOperator, Variable,
    },
};

use super::err::IrError;
use super::{
    builtin::{deinit_builtin_scope, init_builtin_scope},
    id::{
        next_anonymous_id, next_function_id, next_global_id, next_label_id, next_mutable_id,
        next_parameter_id,
    },
    instruction::{
        Comparator, Instruction, IrBinaryOpcode, IrFunctionPrototype, IrScope, IrType, Value,
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

    fn declare_mutable(
        &mut self,
        symbol: String,
        data_type: IrType,
    ) -> Result<Value, CompileError> {
        let id = next_mutable_id();
        let value = Value::StackPointer(format!("v{id}.{symbol}"));
        self.scope.declare(symbol, (value.clone(), data_type))?;
        Ok(value)
    }

    fn declare_global(&mut self, symbol: String, data_type: IrType) -> Result<Value, CompileError> {
        let id = next_global_id();
        let value = Value::GlobalPointer(format!("g{id}.{symbol}"));
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
        statements: Vec<Statement>,
    ) -> Result<Instruction, CompileError> {
        self.scope.enter(Tag::Named("block"));
        let mut instructions = Vec::new();
        for statement in statements {
            instructions.push(self.translate_statement(statement)?);
        }
        self.scope.leave(Tag::Named("block"))?;
        Ok(Instruction::Batch(instructions))
    }

    fn translate_binary_expression(
        &mut self,
        operator: &BinaryOperator,
        sub_type: &Type,
        left: Expression,
        right: Expression,
    ) -> Result<(Instruction, Value), CompileError> {
        let (left_instruction, left) = self.translate_expression(left)?;
        let (right_instruction, right) = self.translate_expression(right)?;
        let result = self.declare_anonymous();

        let operator = match operator {
            BinaryOperator::Add => IrBinaryOpcode::Add,
            BinaryOperator::Subtract => IrBinaryOpcode::Subtract,
            BinaryOperator::Multiply => IrBinaryOpcode::Multiply,
            BinaryOperator::Divide => IrBinaryOpcode::DivideSigned,
            BinaryOperator::LogicalAnd | BinaryOperator::BitAnd => IrBinaryOpcode::And,
            BinaryOperator::LogicalOr | BinaryOperator::BitOr => IrBinaryOpcode::Or,
            BinaryOperator::Equal => IrBinaryOpcode::Compare(Comparator::Equal),
            BinaryOperator::NotEqual => IrBinaryOpcode::Compare(Comparator::NotEqual),
            BinaryOperator::LessThan => IrBinaryOpcode::Compare(Comparator::SignedLessThan),
            BinaryOperator::LessThanEqual => {
                IrBinaryOpcode::Compare(Comparator::SignedLessThanEqual)
            }
            BinaryOperator::GreaterThan => IrBinaryOpcode::Compare(Comparator::SignedGreaterThan),
            BinaryOperator::GreaterThanEqual => {
                IrBinaryOpcode::Compare(Comparator::SignedGreaterThanEqual)
            }
        };

        let data_type = IrType::from(sub_type.clone());

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

    fn translate_unary_expression(
        &mut self,
        operator: &UnaryOperator,
        sub_type: &Type,
        expression: Expression,
    ) -> Result<(Instruction, Value), CompileError> {
        let (expression_instruction, expression_value) = self.translate_expression(expression)?;

        let result = self.declare_anonymous();
        let data_type = IrType::from(sub_type.clone());
        let unary_instruction = match operator {
            UnaryOperator::LogicalNot | UnaryOperator::BitNot => {
                let mask = match data_type {
                    IrType::Bool => 1,
                    IrType::I32 => -1i32,
                    _ => return Err(IrError::MismatchedType.into()),
                };
                Instruction::Binary {
                    operator: IrBinaryOpcode::Xor,
                    data_type,
                    result: result.clone(),
                    left: Value::Immediate(mask),
                    right: expression_value,
                }
            }
            UnaryOperator::Negative => Instruction::Binary {
                operator: IrBinaryOpcode::Subtract,
                data_type,
                result: result.clone(),
                left: Value::Immediate(0),
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
        expression: Expression,
    ) -> Result<(Instruction, Value), CompileError> {
        match expression {
            Expression::Identifier(identifier) => self.scope.lookup_symbol(&identifier)?.map_or(
                Err(IrError::UndeclaredIdentifier(identifier).into()),
                |(value, data_type)| {
                    if let Value::Parameter(_) = value {
                        Ok((Instruction::NoOperation, value))
                    } else {
                        let result = self.declare_anonymous();
                        let instruction = Instruction::Load {
                            data_type,
                            from: value,
                            to: result.clone(),
                        };
                        Ok((instruction, result))
                    }
                },
            ),
            Expression::IntLiteral(literal) => {
                Ok((Instruction::NoOperation, Value::Immediate(literal)))
            }
            Expression::FloatLiteral(_) => todo!(),
            Expression::Unary(operator, sub_type, expression) => {
                self.translate_unary_expression(&operator, &sub_type, *expression)
            }
            Expression::Binary(operator, expression_type, left, right) => {
                self.translate_binary_expression(&operator, &expression_type, *left, *right)
            }
            Expression::Call(function_name, arguments) => {
                let receiver = self.declare_anonymous();
                let Some((function_id, function_type)) =
                    self.scope.lookup_symbol(&function_name)?
                else {
                    return Err(IrError::UndeclaredIdentifier(function_name).into());
                };
                let IrType::Function {
                    return_type,
                    parameter_types: _,
                } = function_type.clone()
                else {
                    return Err(IrError::MismatchedType.into());
                };
                let mut instructions = Vec::new();
                let mut argument_list = Vec::new();
                for argument in arguments {
                    let (instruction, id) = self.translate_expression(argument)?;
                    instructions.push(instruction);
                    argument_list.push(id);
                }
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
                    arguments: argument_list,
                };
                instructions.push(call_instruction);
                Ok((Instruction::Batch(instructions), receiver))
            }
        }
    }

    fn translate_expression_statement(
        &mut self,
        expression: Expression,
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
    fn translate_if_statement(&mut self, if_detail: IfDetail) -> Result<Instruction, CompileError> {
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
        let body_instructions = self.translate_statement(*true_body)?;
        let jump_true_to_end = Instruction::Jump(end.clone());
        let when_false_label = Instruction::Label(when_false);
        let false_body_instructions = match false_body {
            Some(statement) => self.translate_statement(*statement)?,
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
        let_detail: LetDetail,
    ) -> Result<Instruction, CompileError> {
        let LetDetail(Variable(identifier, var_type), expression) = let_detail;
        let data_type = IrType::from(var_type);
        let (expression_instructions, expression) = self.translate_expression(expression)?;
        let assign_instruction = if self.scope.is_global()? {
            let lvalue = self.declare_global(identifier, data_type.clone())?;
            Instruction::Global {
                lvalue,
                data_type,
                value: expression,
            }
        } else {
            let lvalue = self.declare_mutable(identifier, data_type.clone())?;
            Instruction::Batch(vec![
                Instruction::Allocate(lvalue.clone(), data_type.clone()),
                Instruction::Store {
                    data_type,
                    from: expression,
                    to: lvalue,
                },
            ])
        };
        let instructions = vec![expression_instructions, assign_instruction];
        Ok(Instruction::Batch(instructions))
    }

    fn translate_return_statement(
        &mut self,
        return_value: Option<Expression>,
    ) -> Result<Instruction, CompileError> {
        let Some(return_value) = return_value else {
            return Ok(Instruction::ReturnVoid);
        };
        let (expression_instructions, expression) = self.translate_expression(return_value)?;
        let instructions = vec![
            expression_instructions,
            Instruction::Return {
                data_type: IrType::I32,
                value: expression,
            },
        ];
        Ok(Instruction::Batch(instructions))
    }

    fn translate_define_statement(
        &mut self,
        define_detail: DefineDetail,
    ) -> Result<Instruction, CompileError> {
        let DefineDetail {
            prototype:
                FunctionPrototype {
                    identifier,
                    parameters,
                    function_type,
                },
            body,
        } = define_detail;
        let Type::Function {
            return_type,
            parameter_types,
        } = function_type
        else {
            return Err(IrError::MismatchedType.into());
        };
        let parameter_types = parameter_types.into_iter().map(IrType::from).collect();
        let return_type = *return_type;
        let function_type = IrType::Function {
            return_type: Box::new(return_type.into()),
            parameter_types,
        };
        let function = self.declare_function(identifier.clone(), function_type.clone())?;

        self.scope.enter(Tag::Function(identifier.clone()));
        let mut parameter_list = Vec::new();
        for Parameter(identifier) in &parameters {
            parameter_list.push(self.declare_parameter(identifier.clone(), IrType::I32)?);
        }
        let body = Instruction::Batch(vec![
            self.translate_statement(*body)?,
            Instruction::Unreachable,
        ]);
        let body = Box::new(body);
        self.scope.leave(Tag::Function(identifier.clone()))?;

        Ok(Instruction::Definition(
            IrFunctionPrototype {
                function_type,
                id: function,
            },
            parameter_list,
            body,
        ))
    }

    fn translate_statement(&mut self, statement: Statement) -> Result<Instruction, CompileError> {
        match statement {
            Statement::Empty => Ok(Instruction::NoOperation),
            Statement::Block(statements) => self.translate_block_statement(statements),
            Statement::Return(return_value) => self.translate_return_statement(return_value),
            Statement::Expression(expression) => self.translate_expression_statement(expression),
            Statement::If(if_detail) => self.translate_if_statement(if_detail),
            Statement::Let(let_detail) => self.translate_let_statement(let_detail),
            Statement::Define(define_detail) => self.translate_define_statement(define_detail),
        }
    }

    fn translate_program(&mut self, program: Program) -> Result<Instruction, CompileError> {
        let mut instructions = Vec::new();
        for statement in program.statements {
            instructions.push(self.translate_statement(statement)?);
        }
        Ok(Instruction::Batch(instructions))
    }

    fn generate_main(&mut self) -> Result<String, CompileError> {
        let Some((main_id, _)) = self.scope.lookup_symbol(&String::from("main"))? else {
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
    pub fn translate(&mut self, program: Program) -> Result<String, CompileError> {
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
