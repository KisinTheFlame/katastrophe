use crate::compiler::syntax::ast::{
    BinaryOperator, Expression, FunctionPrototype, Program, Statement,
};

use super::{
    builtin::{deinit_builtin_scope, init_builtin_scope},
    err::{IrError, IrErrorKind},
    instruction::{Comparator, Instruction, IrBinaryOpcode, IrFunctionPrototype, IrType, Value},
    scope::{Scope, Tag},
};

pub struct Translator {
    scope: Scope,
}

impl Translator {
    #[must_use]
    pub fn new() -> Translator {
        Translator {
            scope: Scope::new(),
        }
    }

    fn translate_block_statement(&mut self, statement: Statement) -> Result<Instruction, IrError> {
        self.scope.enter(Tag::Named("block"));
        let Statement::Block(statements) = statement else {
            return Err(IrError {
                kind: IrErrorKind::MissingBlock,
            });
        };
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
        left: Expression,
        right: Expression,
    ) -> Result<(Instruction, Value), IrError> {
        let (left_instruction, left) = self.translate_expression(left)?;
        let (right_instruction, right) = self.translate_expression(right)?;
        let result = self.scope.declare_anonymous();

        let operator = match operator {
            BinaryOperator::Add => IrBinaryOpcode::Add,
            BinaryOperator::Subtract => IrBinaryOpcode::Subtract,
            BinaryOperator::Multiply => IrBinaryOpcode::Multiply,
            BinaryOperator::Divide => IrBinaryOpcode::DivideSigned,
            BinaryOperator::LogicalAnd => todo!(),
            BinaryOperator::LogicalOr => todo!(),
            BinaryOperator::BitAnd => todo!(),
            BinaryOperator::BitOr => todo!(),
            BinaryOperator::Equal => IrBinaryOpcode::Compare(Comparator::Equal),
            BinaryOperator::NotEqual => todo!(),
            BinaryOperator::LessThan => IrBinaryOpcode::Compare(Comparator::SignedLessThan),
            BinaryOperator::LessThanEqual => todo!(),
            BinaryOperator::GreaterThan => IrBinaryOpcode::Compare(Comparator::SignedGreaterThan),
            BinaryOperator::GreaterThanEqual => todo!(),
        };

        Ok((
            Instruction::Batch(vec![
                left_instruction,
                right_instruction,
                Instruction::Binary {
                    operator,
                    result: result.clone(),
                    left,
                    right,
                },
            ]),
            result,
        ))
    }

    fn translate_expression(
        &mut self,
        expression: Expression,
    ) -> Result<(Instruction, Value), IrError> {
        match expression {
            Expression::Identifier(identifier) => self.scope.lookup_symbol(&identifier)?.map_or(
                Err(IrError {
                    kind: IrErrorKind::UndeclaredIdentifier(identifier),
                }),
                |(value, _)| {
                    if let Value::Parameter(_) = value {
                        Ok((Instruction::NoOperation, value))
                    } else {
                        let result = self.scope.declare_anonymous();
                        let instruction = Instruction::Load {
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
            Expression::Unary(_, _) => todo!(),
            Expression::Binary(operator, left, right) => {
                self.translate_binary_expression(&operator, *left, *right)
            }
            Expression::Call(function_name, arguments) => {
                let receiver = self.scope.declare_anonymous();
                let Some((function_id, function_type)) =
                    self.scope.lookup_symbol(&function_name)?
                else {
                    return Err(IrError {
                        kind: IrErrorKind::UndeclaredIdentifier(function_name),
                    });
                };
                let IrType::Function {
                    return_type,
                    parameter_types: _,
                } = function_type.clone()
                else {
                    return Err(IrError {
                        kind: IrErrorKind::MismatchedType,
                    });
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
        statement: Statement,
    ) -> Result<Instruction, IrError> {
        let Statement::Expression(expression) = statement else {
            return Err(IrError {
                kind: IrErrorKind::MissingExpression,
            });
        };
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
    fn translate_if_statement(&mut self, statement: Statement) -> Result<Instruction, IrError> {
        let Statement::If {
            condition,
            body,
            else_body,
        } = statement
        else {
            return Err(IrError {
                kind: IrErrorKind::MissingIf,
            });
        };
        let start = self.scope.declare_label();
        let when_true = self.scope.declare_label();
        let when_false = self.scope.declare_label();
        let end = self.scope.declare_label();

        let jump_to_start = Instruction::Jump(start.clone());
        let start_label = Instruction::Label(start);
        let (condition_instructions, condition) = self.translate_expression(condition)?;
        let conditional_jump = Instruction::JumpIf {
            condition,
            when_true: when_true.clone(),
            when_false: when_false.clone(),
        };
        let when_true_label = Instruction::Label(when_true);
        let body_instructions = self.translate_statement(*body)?;
        let jump_true_to_end = Instruction::Jump(end.clone());
        let when_false_label = Instruction::Label(when_false);
        let else_body_instructions = match else_body {
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
            else_body_instructions,
            jump_false_to_end,
            end_label,
        ]))
    }

    fn translate_let_statement(&mut self, statement: Statement) -> Result<Instruction, IrError> {
        let Statement::Let(identifier, expression) = statement else {
            return Err(IrError {
                kind: IrErrorKind::MissingLet,
            });
        };
        let (expression_instructions, expression) = self.translate_expression(expression)?;
        let assign_instruction = if self.scope.is_global()? {
            let lvalue = self.scope.declare_global(&identifier, &IrType::I32)?;
            Instruction::Global {
                lvalue,
                data_type: IrType::I32,
                value: expression,
            }
        } else {
            let lvalue = self.scope.declare_mutable(&identifier, &IrType::I32)?;
            Instruction::Batch(vec![
                Instruction::Allocate(lvalue.clone()),
                Instruction::Store {
                    from: expression,
                    to: lvalue,
                },
            ])
        };
        let instructions = vec![expression_instructions, assign_instruction];
        Ok(Instruction::Batch(instructions))
    }

    fn translate_return_statement(&mut self, statement: Statement) -> Result<Instruction, IrError> {
        let Statement::Return(expression) = statement else {
            return Err(IrError {
                kind: IrErrorKind::MissingReturn,
            });
        };
        let (expression_instructions, expression) = self.translate_expression(expression)?;
        let instructions = vec![
            expression_instructions,
            Instruction::Return {
                data_type: IrType::I32,
                value: expression,
            },
        ];
        Ok(Instruction::Batch(instructions))
    }

    fn translate_define_statement(&mut self, statement: Statement) -> Result<Instruction, IrError> {
        let Statement::Define {
            prototype:
                FunctionPrototype {
                    identifier,
                    parameters,
                    return_type,
                },
            body,
        } = statement
        else {
            return Err(IrError {
                kind: IrErrorKind::MissingDefinition,
            });
        };
        let mut parameter_types = Vec::new();
        for _ in &parameters {
            parameter_types.push(IrType::I32);
        }
        let function_type = IrType::Function {
            return_type: Box::new(return_type.try_into()?),
            parameter_types,
        };
        let function = self.scope.declare_function(&identifier, &function_type)?;

        self.scope.enter(Tag::Function(identifier.clone()));
        let mut parameter_list = Vec::new();
        for parameter in &parameters {
            parameter_list.push(
                self.scope
                    .declare_parameter(&parameter.identifier, &IrType::I32)?,
            );
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

    fn translate_statement(&mut self, statement: Statement) -> Result<Instruction, IrError> {
        match statement {
            Statement::Empty => Ok(Instruction::NoOperation),
            Statement::Block(_) => self.translate_block_statement(statement),
            Statement::Return(_) => self.translate_return_statement(statement),
            Statement::Expression(_) => self.translate_expression_statement(statement),
            Statement::If {
                condition: _,
                body: _,
                else_body: _,
            } => self.translate_if_statement(statement),
            Statement::Let(_, _) => self.translate_let_statement(statement),
            Statement::Define {
                prototype: _,
                body: _,
            } => self.translate_define_statement(statement),
        }
    }

    fn translate_program(&mut self, program: Program) -> Result<Instruction, IrError> {
        let mut instructions = Vec::new();
        for statement in program.statements {
            instructions.push(self.translate_statement(statement)?);
        }
        Ok(Instruction::Batch(instructions))
    }

    fn generate_main(&mut self) -> Result<String, IrError> {
        let Some((main_id, _)) = self.scope.lookup_symbol(&String::from("main"))? else {
            return Err(IrError {
                kind: IrErrorKind::UndefinedMain,
            });
        };
        let template = format!(
            "
define i32 @main() {{
    %main_return = call i32 {main_id}()
    ret i32 %main_return
}}\
            "
        );
        Ok(template)
    }

    /// # Errors
    pub fn translate(&mut self, program: Program) -> Result<String, IrError> {
        let mut llvm_code = String::new();
        init_builtin_scope(&mut self.scope, &mut llvm_code)?;
        self.scope.enter(Tag::Global);
        let user_program = self.translate_program(program)?.to_string();
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
