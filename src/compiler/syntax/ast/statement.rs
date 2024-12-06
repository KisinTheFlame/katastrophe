use std::fmt;
use std::rc::Rc;

use crate::util::common::Array;
use crate::util::pretty_format::PrettyFormat;
use crate::util::pretty_format::indent;

use super::crumb::FunctionPrototype;
use super::crumb::Variable;
use super::expression::Expression;
use super::package::UsingPath;
use super::ty::Type;

pub struct IfDetail {
    pub condition: Rc<Expression>,
    pub true_body: Rc<Statement>,
    pub false_body: Option<Rc<Statement>>,
}

pub struct LetDetail(pub Variable, pub Rc<Expression>);

pub struct WhileDetail(pub Rc<Expression>, pub Rc<Statement>);

pub struct DefineDetail {
    pub prototype: Rc<FunctionPrototype>,
    pub builtin: bool,
    pub body: Rc<Statement>,
}

pub enum Statement {
    Empty,
    Block(Array<Statement>),
    Return(Option<Rc<Expression>>),
    Expression(Rc<Expression>),
    If(IfDetail),
    Let(LetDetail),
    While(WhileDetail),
    Define(DefineDetail),
    Using(UsingPath),
}

impl PrettyFormat for Statement {
    fn pretty_format(&self, f: &mut fmt::Formatter, indentation_num: usize) -> fmt::Result {
        let indent = indent(indentation_num);
        match self {
            Statement::Empty => {}
            Statement::Block(statements) => {
                statements
                    .iter()
                    .try_for_each(|statement| statement.pretty_format(f, indentation_num))?;
            }
            Statement::Return(expression) => {
                writeln!(f, "{indent}Return")?;
                expression.as_ref().map(|e| e.pretty_format(f, indentation_num + 1));
            }
            Statement::Expression(expression) => {
                expression.pretty_format(f, indentation_num)?;
            }
            Statement::If(IfDetail {
                condition,
                true_body: body,
                false_body: else_body,
            }) => {
                writeln!(f, "{indent}If")?;
                condition.pretty_format(f, indentation_num + 1)?;
                writeln!(f, "{indent}Then")?;
                body.pretty_format(f, indentation_num + 1)?;
                if let Some(else_body) = else_body {
                    writeln!(f, "{indent}Else")?;
                    else_body.pretty_format(f, indentation_num + 1)?;
                }
            }
            Statement::While(WhileDetail(condition_expression, body)) => {
                writeln!(f, "{indent}While")?;
                condition_expression.pretty_format(f, indentation_num + 1)?;
                writeln!(f, "{indent}Do")?;
                body.pretty_format(f, indentation_num + 1)?;
            }
            Statement::Let(LetDetail(variable, expression)) => {
                let Variable(id, var_type, mutability) = variable;
                writeln!(f, "{indent}Let {mutability} {id} as {var_type}")?;
                expression.pretty_format(f, indentation_num + 1)?;
            }
            Statement::Define(DefineDetail {
                prototype,
                builtin,
                body,
            }) => {
                let FunctionPrototype {
                    identifier,
                    parameters,
                    function_type,
                } = prototype.as_ref();

                let Type::Function {
                    return_type,
                    parameter_types,
                } = function_type.as_ref()
                else {
                    panic!("must be a function type");
                };
                let parameters = parameters
                    .iter()
                    .zip(parameter_types.iter())
                    .map(|(parameter, param_type)| format!("{parameter} as {param_type}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                let builtin = if *builtin { "builtin " } else { "" };
                writeln!(f, "{indent}Define {builtin}{identifier}({parameters}) -> {return_type}")?;
                body.pretty_format(f, indentation_num + 1)?;
            }
            Statement::Using(path) => {
                writeln!(f, "{indent}Using {path}")?;
            }
        };
        Ok(())
    }
}
