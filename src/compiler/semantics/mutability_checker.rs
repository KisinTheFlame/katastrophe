use crate::compiler::{
    err::CompileError,
    scope::{Scope, Tag},
    syntax::ast::{
        crumb::{FunctionPrototype, Mutability, Parameter, Variable},
        expression::Expression,
        operator::Binary,
        statement::{DefineDetail, LetDetail, Statement},
        Program,
    },
};

use super::err::SemanticError;

type MutabilityScope = Scope<Mutability>;

pub struct MutabilityChecker {
    scope: MutabilityScope,
}

impl MutabilityChecker {
    #[must_use]
    pub fn new() -> MutabilityChecker {
        MutabilityChecker {
            scope: Scope::new(),
        }
    }

    /// # Errors
    pub fn check_program(&mut self, program: &Program) -> Result<(), CompileError> {
        self.scope.enter(Tag::Global);
        program
            .statements
            .iter()
            .try_for_each(|statement| self.check_statement(statement))?;
        self.scope.leave(Tag::Global)?;
        Ok(())
    }

    fn check_statement(&mut self, statement: &Statement) -> Result<(), CompileError> {
        match statement {
            Statement::Empty | Statement::Return(_) => Ok(()),
            Statement::Block(statements) => statements
                .iter()
                .try_for_each(|statement| self.check_statement(statement)),
            Statement::Define(DefineDetail {
                prototype:
                    FunctionPrototype {
                        identifier,
                        parameters,
                        function_type: _,
                    },
                body,
            }) => {
                self.scope.enter(Tag::Function(identifier.clone()));
                parameters.iter().try_for_each(|Parameter(identifier)| {
                    self.scope
                        .declare(identifier.clone(), Mutability::Immutable)
                })?;
                self.check_statement(body)?;
                self.scope.leave(Tag::Function(identifier.clone()))?;
                Ok(())
            }
            Statement::If(if_detail) => {
                self.scope.enter(Tag::Anonymous);
                let true_result = self.check_statement(&if_detail.true_body);
                self.scope.leave(Tag::Anonymous)?;

                let false_result = if_detail.false_body.as_ref().map_or(Ok(()), |body| {
                    self.scope.enter(Tag::Anonymous);
                    self.check_statement(body)?;
                    self.scope.leave(Tag::Anonymous)?;
                    Ok(())
                });

                true_result.and(false_result)
            }
            Statement::Let(LetDetail(Variable(identifier, _, mutability), _)) => {
                if self.scope.is_global()? {
                    self.scope.declare(identifier.clone(), *mutability)?;
                } else {
                    self.scope.overwrite(identifier.clone(), *mutability)?;
                }
                Ok(())
            }
            Statement::Expression(expression) => match expression {
                Expression::Binary(Binary::Assign, _, lvalue, _) => self.check_lvalue(lvalue),
                _ => Ok(()),
            },
        }
    }

    fn check_lvalue(&self, lvalue: &Expression) -> Result<(), CompileError> {
        match lvalue {
            Expression::Identifier(identifier) => {
                let Some(mutability) = self.scope.lookup(identifier)? else {
                    return Err(SemanticError::UndeclaredIdentifier(identifier.clone()).into());
                };
                if mutability == Mutability::Mutable {
                    Ok(())
                } else {
                    Err(SemanticError::AssigningImmutableVariable(identifier.clone()).into())
                }
            }
            Expression::IntLiteral(_)
            | Expression::FloatLiteral(_)
            | Expression::BoolLiteral(_)
            | Expression::Unary(_, _, _)
            | Expression::Binary(_, _, _, _)
            | Expression::Call(_, _) => Err(SemanticError::IllegalLValue.into()),
        }
    }
}

impl Default for MutabilityChecker {
    fn default() -> Self {
        Self::new()
    }
}
