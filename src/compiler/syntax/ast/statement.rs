use crate::util::pretty_format::{indent, PrettyFormat};

use super::{crumb::FunctionPrototype, crumb::Variable, expression::Expression, Type};

pub struct IfDetail {
    pub condition: Expression,
    pub true_body: Box<Statement>,
    pub false_body: Option<Box<Statement>>,
}

pub struct LetDetail(pub Variable, pub Expression);

pub struct DefineDetail {
    pub prototype: FunctionPrototype,
    pub body: Box<Statement>,
}

pub enum Statement {
    Empty,
    Block(Vec<Statement>),
    Return(Option<Expression>),
    Expression(Expression),
    If(IfDetail),
    Let(LetDetail),
    Define(DefineDetail),
}

impl PrettyFormat for Statement {
    fn pretty_format(
        &self,
        f: &mut std::fmt::Formatter,
        indentation_num: usize,
    ) -> std::fmt::Result {
        let indentation = indent(indentation_num);
        match self {
            Statement::Empty => {}
            Statement::Block(statements) => {
                statements
                    .iter()
                    .try_for_each(|statement| statement.pretty_format(f, indentation_num))?;
            }
            Statement::Return(expression) => {
                writeln!(f, "{indentation}Return")?;
                expression
                    .as_ref()
                    .map(|e| e.pretty_format(f, indentation_num + 1));
            }
            Statement::Expression(expression) => {
                expression.pretty_format(f, indentation_num)?;
            }
            Statement::If(IfDetail {
                condition,
                true_body: body,
                false_body: else_body,
            }) => {
                writeln!(f, "{indentation}If")?;
                condition.pretty_format(f, indentation_num + 1)?;
                writeln!(f, "{indentation}Then")?;
                body.pretty_format(f, indentation_num + 1)?;
                if let Some(else_body) = else_body {
                    writeln!(f, "{indentation}Else")?;
                    else_body.pretty_format(f, indentation_num + 1)?;
                }
            }
            Statement::Let(LetDetail(variable, expression)) => {
                let Variable(id, var_type, mutability) = variable;
                writeln!(f, "{indentation}Let {mutability} {id} as {var_type}")?;
                expression.pretty_format(f, indentation_num + 1)?;
            }
            Statement::Define(DefineDetail {
                prototype:
                    FunctionPrototype {
                        identifier,
                        parameters,
                        function_type,
                    },
                body,
            }) => {
                let Type::Function {
                    return_type,
                    parameter_types,
                } = function_type
                else {
                    panic!("must be a function type");
                };
                let parameters = parameters
                    .iter()
                    .zip(parameter_types)
                    .map(|(parameter, param_type)| format!("{parameter} as {param_type}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                writeln!(
                    f,
                    "{indentation}Define {identifier}({parameters}) -> {return_type}"
                )?;
                body.pretty_format(f, indentation_num + 1)?;
            }
        };
        Ok(())
    }
}
