use std::rc::Rc;

use crate::CompileResult;
use crate::compiler::context::Context;
use crate::compiler::context::DocumentId;
use crate::compiler::err::CompileError;
use crate::compiler::err::IceUnwrap;
use crate::compiler::scope::Scope;
use crate::compiler::scope::Tag;
use crate::compiler::syntax::ast::crumb::FunctionPrototype;
use crate::compiler::syntax::ast::crumb::Mutability;
use crate::compiler::syntax::ast::crumb::Parameter;
use crate::compiler::syntax::ast::crumb::Variable;
use crate::compiler::syntax::ast::expression::Expression;
use crate::compiler::syntax::ast::lvalue::LValue;
use crate::compiler::syntax::ast::operator::Binary;
use crate::compiler::syntax::ast::package::UsingPath;
use crate::compiler::syntax::ast::reference::Reference;
use crate::compiler::syntax::ast::statement::DefineDetail;
use crate::compiler::syntax::ast::statement::LetDetail;
use crate::compiler::syntax::ast::statement::Statement;
use crate::compiler::syntax::ast::statement::WhileDetail;
use crate::sys_error;

type MutabilityScope = Scope<Mutability>;

pub struct LValueChecker {
    scope: MutabilityScope,
}

impl LValueChecker {
    #[must_use]
    pub fn new() -> LValueChecker {
        LValueChecker { scope: Scope::new() }
    }

    /// # Errors
    pub fn check_document(&mut self, context: &Context, document_id: DocumentId) -> CompileResult<()> {
        let Some(document) = context.document_map.get(&document_id) else {
            sys_error!("document must exist");
        };
        let _global_scope = self.scope.enter(Tag::Global);
        document
            .statements
            .iter()
            .try_for_each(|statement| self.check_statement(context, statement))?;
        Ok(())
    }

    fn check_statement(&mut self, context: &Context, statement: &Statement) -> CompileResult<()> {
        match statement {
            Statement::Empty | Statement::Return(_) | Statement::Struct(_) => Ok(()),
            Statement::Block(statements) => {
                let _block_scope = self.scope.enter(Tag::Anonymous);
                statements
                    .iter()
                    .try_for_each(|statement| self.check_statement(context, statement))?;
                Ok(())
            }
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
                let _function_scope = self.scope.enter(Tag::Function(identifier.clone()));
                parameters
                    .iter()
                    .map(Rc::as_ref)
                    .try_for_each(|Parameter(parameter_id)| {
                        self.scope.declare(parameter_id.clone(), Mutability::Immutable)
                    })?;
                self.check_statement(context, body)?;
                Ok(())
            }
            Statement::If(if_detail) => {
                {
                    let _true_scope = self.scope.enter(Tag::Anonymous);
                    self.check_statement(context, &if_detail.true_body)?;
                }
                if_detail.false_body.as_ref().map_or(Ok(()), |body| {
                    let _false_scope = self.scope.enter(Tag::Anonymous);
                    self.check_statement(context, body)?;
                    Ok(())
                })
            }
            Statement::While(WhileDetail(_, body)) => {
                let _while_scope = self.scope.enter(Tag::Named("while"));
                self.check_statement(context, body)?;
                Ok(())
            }
            Statement::Let(LetDetail(Variable(identifier, _, mutability), _)) => {
                if self.scope.is_global() {
                    self.scope.declare(identifier.clone(), *mutability)?;
                } else {
                    self.scope.overwrite(identifier.clone(), *mutability);
                }
                Ok(())
            }
            Statement::Expression(expression) => match expression.as_ref() {
                Expression::Binary(Binary::Assign, _, lvalue, _) => self.check_lvalue(lvalue),
                _ => Ok(()),
            },
            Statement::Using(UsingPath(document_path, symbol)) => {
                let id = context
                    .id_map
                    .get(document_path)
                    .or_ice("using path should be registered during parsing");
                let Some(reference) = context
                    .reference_map
                    .get(id)
                    .or_ice("reference map should be populated during parsing")
                    .get(symbol)
                else {
                    sys_error!("used symbol must exist");
                };
                if let Reference::Binding(_, mutability) = reference.as_ref() {
                    self.scope.declare(symbol.clone(), *mutability)?;
                }
                Ok(())
            }
        }
    }

    fn check_lvalue(&self, lvalue_expression: &Expression) -> CompileResult<()> {
        let lvalue = LValue::try_from(lvalue_expression)?;
        let identifier = lvalue.root();

        let Some(mutability) = self.scope.lookup(&identifier) else {
            return Err(CompileError::UndeclaredIdentifier(identifier.clone()));
        };
        if mutability == Mutability::Mutable {
            Ok(())
        } else {
            Err(CompileError::AssigningImmutableVariable(identifier.clone()))
        }
    }
}

impl Default for LValueChecker {
    fn default() -> Self {
        Self::new()
    }
}
