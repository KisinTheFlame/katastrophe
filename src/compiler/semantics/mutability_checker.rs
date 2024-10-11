use std::rc::Rc;

use crate::{
    compiler::{
        context::{Context, DocumentId},
        err::CompileError,
        scope::{Scope, Tag},
        syntax::ast::{
            crumb::{FunctionPrototype, Mutability, Parameter, Variable},
            expression::Expression,
            operator::Binary,
            package::UsingPath,
            statement::{DefineDetail, LetDetail, Statement, WhileDetail},
        },
    },
    sys_error,
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
    pub fn check_document(
        &mut self,
        context: &Context,
        document_id: DocumentId,
    ) -> Result<(), CompileError> {
        let Some(document) = context.document_map.get(&document_id) else {
            return sys_error!("document must exist");
        };
        self.scope.enter(Tag::Global);
        document
            .statements
            .iter()
            .try_for_each(|statement| self.check_statement(context, statement))?;
        self.scope.leave(Tag::Global)?;
        Ok(())
    }

    fn check_statement(
        &mut self,
        context: &Context,
        statement: &Statement,
    ) -> Result<(), CompileError> {
        match statement {
            Statement::Empty | Statement::Return(_) => Ok(()),
            Statement::Block(statements) => statements
                .iter()
                .try_for_each(|statement| self.check_statement(context, statement)),
            Statement::Define(DefineDetail {
                prototype,
                builtin,
                body,
            }) => {
                let FunctionPrototype {
                    identifier,
                    parameters,
                    function_type: _,
                } = prototype.as_ref();

                if *builtin {
                    return Ok(());
                }
                self.scope.enter(Tag::Function(identifier.clone()));
                parameters
                    .iter()
                    .map(Rc::as_ref)
                    .try_for_each(|Parameter(parameter_id)| {
                        self.scope
                            .declare(parameter_id.clone(), Mutability::Immutable)
                    })?;
                self.check_statement(context, body)?;
                self.scope.leave(Tag::Function(identifier.clone()))?;
                Ok(())
            }
            Statement::If(if_detail) => {
                self.scope.enter(Tag::Anonymous);
                self.check_statement(context, &if_detail.true_body)?;
                self.scope.leave(Tag::Anonymous)?;

                if_detail.false_body.as_ref().map_or(Ok(()), |body| {
                    self.scope.enter(Tag::Anonymous);
                    self.check_statement(context, body)?;
                    self.scope.leave(Tag::Anonymous)?;
                    Ok(())
                })
            }
            Statement::While(WhileDetail(_, body)) => {
                self.scope.enter(Tag::Named("while"));
                self.check_statement(context, body)?;
                self.scope.leave(Tag::Named("while"))?;
                Ok(())
            }
            Statement::Let(LetDetail(Variable(identifier, _, mutability), _)) => {
                if self.scope.is_global()? {
                    self.scope.declare(identifier.clone(), *mutability)?;
                } else {
                    self.scope.overwrite(identifier.clone(), *mutability)?;
                }
                Ok(())
            }
            Statement::Expression(expression) => match expression.as_ref() {
                Expression::Binary(Binary::Assign, _, lvalue, _) => self.check_lvalue(lvalue),
                _ => Ok(()),
            },
            Statement::Using(UsingPath(document_path, symbol)) => {
                let id = context.id_map.get(document_path).unwrap();
                let Some(mutability) = context.mutability_map.get(id).unwrap().get(symbol) else {
                    return sys_error!("used symbol must exist");
                };
                self.scope.declare(symbol.clone(), *mutability)?;
                Ok(())
            }
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
