use crate::compiler::syntax::ast::{
    BinaryOperator, Expression, FunctionPrototype, Program, Statement,
};

use super::{
    builtin::{deinit_builtin_scope, init_builtin_scope},
    err::{IrError, IrErrorKind},
    instruction::{BinaryOperation, DataType, Instruction, Value},
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

        let operation = match operator {
            BinaryOperator::Add => Instruction::Binary {
                operator: BinaryOperation::Add,
                result: result.clone(),
                left,
                right,
            },
            BinaryOperator::Subtract => Instruction::Binary {
                operator: BinaryOperation::Subtract,
                result: result.clone(),
                left,
                right,
            },
            BinaryOperator::Multiply => Instruction::Binary {
                operator: BinaryOperation::Multiply,
                result: result.clone(),
                left,
                right,
            },
            BinaryOperator::Divide => Instruction::Binary {
                operator: BinaryOperation::DivideSigned,
                result: result.clone(),
                left,
                right,
            },
            BinaryOperator::LogicalAnd => todo!(),
            BinaryOperator::LogicalOr => todo!(),
            BinaryOperator::BitAnd => todo!(),
            BinaryOperator::BitOr => todo!(),
            BinaryOperator::Equal => todo!(),
            BinaryOperator::NotEqual => todo!(),
            BinaryOperator::LessThan => todo!(),
            BinaryOperator::LessThanEqual => todo!(),
            BinaryOperator::GreaterThan => todo!(),
            BinaryOperator::GreaterThanEqual => todo!(),
        };

        Ok((
            Instruction::Batch(vec![left_instruction, right_instruction, operation]),
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
                    kind: IrErrorKind::UndeclaredIdentifier,
                }),
                |pointer| {
                    let result = self.scope.declare_anonymous();
                    let instruction = Instruction::Load {
                        from: pointer,
                        to: result.clone(),
                    };
                    Ok((instruction, result))
                },
            ),
            Expression::IntLiteral(literal) => {
                // let symbol = &format!("{literal}");
                // // if self.scope.exist_symbol(symbol)? {
                // //     let expression = self.scope.lookup_symbol(symbol)?.unwrap();
                // //     Ok((Instruction::NoOperation, expression))
                // // } else {
                // //     let expression = self.scope.declare_immutable(symbol)?;
                // //     Ok((
                // //         Instruction::Bitcast {
                // //             from: Value::Immediate(literal),
                // //             to: expression,
                // //         },
                // //         expression,
                // //     ))
                // // }
                Ok((Instruction::NoOperation, Value::Immediate(literal)))
            }
            Expression::FloatLiteral(_) => todo!(),
            Expression::Unary(_, _) => todo!(),
            Expression::Binary(operator, left, right) => {
                self.translate_binary_expression(&operator, *left, *right)
            }
            Expression::Call(_, _) => todo!(),
        }
    }

    fn translate_expression_statement(&self, statement: Statement) -> Result<Instruction, IrError> {
        let Statement::Expression(expression) = statement else {
            return Err(IrError {
                kind: IrErrorKind::MissingExpression,
            });
        };
        match expression {
            Expression::Identifier(_) => todo!(),
            Expression::IntLiteral(_) => todo!(),
            Expression::FloatLiteral(_) => todo!(),
            Expression::Unary(_, _) => todo!(),
            Expression::Binary(_, _, _) => todo!(),
            Expression::Call(_, _) => todo!(),
        }
    }

    fn translate_if_statement(&self, statement: Statement) -> Result<Instruction, IrError> {
        drop(statement);
        todo!()
    }

    fn translate_let_statement(&mut self, statement: Statement) -> Result<Instruction, IrError> {
        let Statement::Let(identifier, expression) = statement else {
            return Err(IrError {
                kind: IrErrorKind::MissingLet,
            });
        };
        let (expression_instructions, expression) = self.translate_expression(expression)?;
        let assign_instruction = if self.scope.is_global()? {
            let lvalue = self.scope.declare_global(&identifier)?;
            Instruction::Global {
                lvalue,
                data_type: DataType::I32,
                value: expression,
            }
        } else {
            let lvalue = self.scope.declare_mutable(&identifier)?;
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
                data_type: DataType::I32,
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
                    // todo handle parameters
                    parameters: _,
                },
            body,
        } = statement
        else {
            return Err(IrError {
                kind: IrErrorKind::MissingDefinition,
            });
        };
        let function = self.scope.declare_function(&identifier)?;
        let tag_name = format!("function {identifier}");
        self.scope.enter(Tag::Dynamic(tag_name.clone()));
        let body = self.translate_statement(*body)?;
        let body = Box::new(body);
        self.scope.leave(Tag::Dynamic(tag_name.clone()))?;
        Ok(Instruction::Function {
            return_type: DataType::I32,
            id: function,
            parameters: Vec::new(),
            body,
        })
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
        let Some(main_id) = self.scope.lookup_symbol(&String::from("main"))? else {
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
