use std::collections::HashMap;
use std::mem;
use std::rc::Rc;

use crate::compiler::context::next_document_id;
use crate::compiler::context::Context;
use crate::compiler::context::DocumentId;
use crate::compiler::err::CompileError;
use crate::compiler::lexis::lexer::Lexer;
use crate::compiler::lexis::token::Keyword;
use crate::compiler::lexis::token::Symbol;
use crate::compiler::lexis::token::Token;
use crate::compiler::scope::Scope;
use crate::compiler::scope::Tag;
use crate::compiler::syntax::ast::package::load_package_path;
use crate::util::common::Array;

use super::ast::crumb::FunctionPrototype;
use super::ast::crumb::Identifier;
use super::ast::crumb::Mutability;
use super::ast::crumb::Parameter;
use super::ast::crumb::Variable;
use super::ast::expression::Expression;
use super::ast::operator::Binary;
use super::ast::operator::Operator;
use super::ast::operator::Unary;
use super::ast::package::DocumentPath;
use super::ast::package::UsingPath;
use super::ast::statement::DefineDetail;
use super::ast::statement::IfDetail;
use super::ast::statement::LetDetail;
use super::ast::statement::Statement;
use super::ast::statement::WhileDetail;
use super::ast::ty::Type;
use super::ast::Document;

pub struct Parser {
    lexer: Lexer,
    document_path: Rc<DocumentPath>,
    type_map: HashMap<Rc<Identifier>, Rc<Type>>,
    mutability_map: HashMap<Rc<Identifier>, Mutability>,
    scope: Scope<()>,
}

type TypedParameters = (Array<Rc<Parameter>>, Array<Rc<Type>>);

impl Parser {
    #[must_use]
    pub fn new(document_path: Rc<DocumentPath>, code: &str) -> Parser {
        Parser {
            lexer: Lexer::new(code),
            document_path,
            type_map: HashMap::new(),
            mutability_map: HashMap::new(),
            scope: Scope::new(),
        }
    }

    fn match_keyword(&mut self, expected_keyword: Keyword) -> bool {
        if let Some(Token::Keyword(keyword)) = self.lexer.peek() {
            if *keyword == expected_keyword {
                return true;
            }
        }
        false
    }

    fn expect_keyword(&mut self, expected_keyword: Keyword) -> bool {
        let matched = self.match_keyword(expected_keyword);
        if matched {
            self.lexer.next();
        }
        matched
    }

    fn digest_keyword(&mut self, expected_keyword: Keyword) -> Result<(), CompileError> {
        if self.expect_keyword(expected_keyword) {
            Ok(())
        } else {
            Err(CompileError::MissingKeyword(expected_keyword))
        }
    }

    fn match_symbol(&mut self, expected_symbol: Symbol) -> bool {
        if let Some(Token::Symbol(symbol)) = self.lexer.peek() {
            if *symbol == expected_symbol {
                return true;
            }
        }
        false
    }

    fn expect_symbol(&mut self, expected_symbol: Symbol) -> bool {
        let matched = self.match_symbol(expected_symbol);
        if matched {
            self.lexer.next();
        }
        matched
    }

    fn digest_symbol(&mut self, expected_symbol: Symbol) -> Result<(), CompileError> {
        if self.expect_symbol(expected_symbol) {
            Ok(())
        } else {
            Err(CompileError::MissingSymbol(expected_symbol))
        }
    }

    fn parse_function_call_args(&mut self) -> Result<Array<Rc<Expression>>, CompileError> {
        self.digest_symbol(Symbol::LeftParentheses)?;
        if self.expect_symbol(Symbol::RightParentheses) {
            return Ok([].into());
        }
        let mut args = Vec::new();
        loop {
            let arg = self.parse_expression()?;
            args.push(arg.into());
            if !self.expect_symbol(Symbol::Comma) {
                break;
            }
        }
        self.digest_symbol(Symbol::RightParentheses)?;
        Ok(args.into())
    }

    fn parse_plain_identifier(&mut self) -> Result<String, CompileError> {
        if let Some(Token::Identifier(identifier)) = self.lexer.peek() {
            let identifier = identifier.clone();
            self.lexer.next();
            Ok(identifier)
        } else {
            Err(CompileError::MissingIdentifier)
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression, CompileError> {
        let identifier = self.parse_plain_identifier()?;
        if self.match_symbol(Symbol::LeftParentheses) {
            Ok(Expression::Call(
                identifier.into(),
                self.parse_function_call_args()?,
            ))
        } else {
            Ok(Expression::Identifier(identifier.into()))
        }
    }

    fn parse_integer_literal(&mut self, literal: i32) -> Expression {
        self.lexer.next();
        Expression::IntLiteral(literal)
    }

    fn parse_char_literal(&mut self, literal: char) -> Expression {
        self.lexer.next();
        Expression::CharLiteral(literal)
    }

    fn parse_float_literal(&mut self, literal: f64) -> Expression {
        self.lexer.next();
        Expression::FloatLiteral(literal)
    }

    fn parse_bool_literal(&mut self, literal: bool) -> Expression {
        self.lexer.next();
        Expression::BoolLiteral(literal)
    }

    fn parse_unary_expression(&mut self) -> Result<Expression, CompileError> {
        if let Some(Token::Symbol(symbol)) = self.lexer.peek() {
            let operator = match symbol {
                Symbol::Subtract => Unary::Negative,
                Symbol::LogicalNot => Unary::LogicalNot,
                Symbol::BitNot => Unary::BitNot,
                symbol => {
                    return Err(CompileError::MissingSymbol(*symbol));
                }
            };
            self.lexer.next();
            Ok(Expression::Unary(
                operator,
                Type::Unknown.into(),
                self.parse_primary()?.into(),
            ))
        } else {
            let kind = if let Some(token) = self.lexer.peek() {
                CompileError::UnexpectedToken(token.clone())
            } else {
                CompileError::UnexpectedParseEOF
            };
            Err(kind)
        }
    }

    fn parse_primary(&mut self) -> Result<Expression, CompileError> {
        match self.lexer.peek() {
            Some(Token::Identifier(_)) => Ok(self.parse_identifier()?),
            Some(Token::IntLiteral(literal)) => {
                let literal = *literal;
                Ok(self.parse_integer_literal(literal))
            }
            Some(Token::CharLiteral(literal)) => {
                let literal = *literal;
                Ok(self.parse_char_literal(literal))
            }
            Some(Token::FloatLiteral(literal)) => {
                let literal = *literal;
                Ok(self.parse_float_literal(literal))
            }
            Some(Token::BoolLiteral(literal)) => {
                let literal = *literal;
                Ok(self.parse_bool_literal(literal))
            }
            Some(Token::Symbol(Symbol::LeftParentheses)) => {
                Ok(self.parse_parentheses_expression()?)
            }
            Some(_) => Ok(self.parse_unary_expression()?),
            None => Err(CompileError::UnexpectedParseEOF),
        }
    }

    fn peek_binary_operator(&mut self) -> Option<Binary> {
        if let Some(Token::Keyword(Keyword::As)) = self.lexer.peek() {
            return Some(Binary::As);
        }
        let Some(Token::Symbol(symbol)) = self.lexer.peek() else {
            return None;
        };
        match symbol {
            Symbol::Add => Some(Binary::Add),
            Symbol::Subtract => Some(Binary::Subtract),
            Symbol::Multiply => Some(Binary::Multiply),
            Symbol::Divide => Some(Binary::Divide),
            Symbol::LogicalAnd => Some(Binary::LogicalAnd),
            Symbol::LogicalOr => Some(Binary::LogicalOr),
            Symbol::BitAnd => Some(Binary::BitAnd),
            Symbol::BitOr => Some(Binary::BitOr),
            Symbol::LeftShift => Some(Binary::LeftShift),
            Symbol::RightShift => Some(Binary::RightShift),
            Symbol::Equal => Some(Binary::Equal),
            Symbol::NotEqual => Some(Binary::NotEqual),
            Symbol::LessThan => Some(Binary::LessThan),
            Symbol::LessThanEqual => Some(Binary::LessThanEqual),
            Symbol::GreaterThan => Some(Binary::GreaterThan),
            Symbol::GreaterThanEqual => Some(Binary::GreaterThanEqual),
            Symbol::Assign => Some(Binary::Assign),
            Symbol::LogicalNot
            | Symbol::BitNot
            | Symbol::LeftParentheses
            | Symbol::RightParentheses
            | Symbol::LeftBracket
            | Symbol::RightBracket
            | Symbol::LeftBrace
            | Symbol::RightBrace
            | Symbol::Comma
            | Symbol::Semicolon
            | Symbol::DoubleColon
            | Symbol::Arrow => None,
        }
    }

    fn parse_binary_expression_rhs(
        &mut self,
        last_precedence: u8,
        mut lhs: Expression,
    ) -> Result<Expression, CompileError> {
        loop {
            let Some(current_operator) = self.peek_binary_operator() else {
                return Ok(lhs);
            };

            if current_operator.precedence() < last_precedence {
                return Ok(lhs);
            }

            self.lexer.next();

            if current_operator == Binary::As {
                let to_type = self.parse_type()?;
                lhs = Expression::Cast(Rc::new(lhs), Type::Unknown.into(), to_type.into());
                continue;
            }

            let rhs = self.parse_primary()?;

            if !current_operator.is_left_associative() {
                return Ok(Expression::Binary(
                    current_operator,
                    Type::Unknown.into(),
                    Rc::new(lhs),
                    Rc::new(self.parse_binary_expression_rhs(0, rhs)?),
                ));
            }

            let Some(next_operator) = self.peek_binary_operator() else {
                return Ok(Expression::Binary(
                    current_operator,
                    Type::Unknown.into(),
                    Rc::new(lhs),
                    Rc::new(rhs),
                ));
            };
            let need_to_associate_with_right = if next_operator.is_left_associative() {
                next_operator.precedence() > current_operator.precedence()
            } else {
                next_operator.precedence() == current_operator.precedence()
            };
            let rhs = if need_to_associate_with_right {
                self.parse_binary_expression_rhs(next_operator.precedence(), rhs)?
            } else {
                rhs
            };

            lhs = Expression::Binary(
                current_operator,
                Type::Unknown.into(),
                Rc::new(lhs),
                Rc::new(rhs),
            );
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, CompileError> {
        let lhs = self.parse_primary()?;

        self.parse_binary_expression_rhs(0, lhs)
    }

    fn parse_parentheses_expression(&mut self) -> Result<Expression, CompileError> {
        self.digest_symbol(Symbol::LeftParentheses)?;
        let expression = self.parse_expression()?;
        self.digest_symbol(Symbol::RightParentheses)?;
        Ok(expression)
    }

    fn parse_empty_statement(&mut self) -> Result<Statement, CompileError> {
        self.digest_symbol(Symbol::Semicolon)?;
        Ok(Statement::Empty)
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, CompileError> {
        let expression = self.parse_expression()?;
        self.digest_symbol(Symbol::Semicolon)?;
        Ok(Statement::Expression(expression.into()))
    }

    fn parse_block_statement(&mut self, context: &mut Context) -> Result<Statement, CompileError> {
        let mut statements = Vec::new();
        self.digest_symbol(Symbol::LeftBrace)?;
        while !self.expect_symbol(Symbol::RightBrace) {
            let statement = self.parse_statement(context)?.into();
            statements.push(statement);
        }
        Ok(Statement::Block(statements.into()))
    }

    fn parse_if_statement(&mut self, context: &mut Context) -> Result<IfDetail, CompileError> {
        self.digest_keyword(Keyword::If)?;
        let condition = self.parse_expression()?.into();
        let true_body = self.parse_block_statement(context)?.into();
        let false_body = if self.expect_keyword(Keyword::Else) {
            if self.match_keyword(Keyword::If) {
                Some(Statement::If(self.parse_if_statement(context)?).into())
            } else {
                Some(self.parse_block_statement(context)?.into())
            }
        } else {
            None
        };
        Ok(IfDetail {
            condition,
            true_body,
            false_body,
        })
    }

    fn parse_while_statement(
        &mut self,
        context: &mut Context,
    ) -> Result<WhileDetail, CompileError> {
        self.digest_keyword(Keyword::While)?;
        let condition = self.parse_expression()?.into();
        let body = self.parse_block_statement(context)?.into();
        Ok(WhileDetail(condition, body))
    }

    fn parse_function_parameter(&mut self) -> Result<(Parameter, Type), CompileError> {
        let identifier = self.parse_plain_identifier()?;
        self.digest_keyword(Keyword::As)?;
        let parameter_type = self.parse_type()?;
        Ok((Parameter(identifier.into()), parameter_type))
    }

    fn parse_function_parameters(&mut self) -> Result<TypedParameters, CompileError> {
        self.digest_symbol(Symbol::LeftParentheses)?;
        if self.expect_symbol(Symbol::RightParentheses) {
            return Ok(([].into(), [].into()));
        }
        let mut parameters = Vec::new();
        let mut parameter_types = Vec::new();
        loop {
            let (parameter, parameter_type) = self.parse_function_parameter()?;
            parameters.push(Rc::new(parameter));
            parameter_types.push(parameter_type.into());
            if !self.expect_symbol(Symbol::Comma) {
                break;
            }
        }
        self.digest_symbol(Symbol::RightParentheses)?;
        Ok((parameters.into(), parameter_types.into()))
    }

    fn parse_function_type(&mut self) -> Result<Type, CompileError> {
        let mut parameter_types = Vec::new();
        loop {
            if self.expect_symbol(Symbol::RightParentheses) {
                break;
            }
            parameter_types.push(self.parse_type()?.into());
            if !self.expect_symbol(Symbol::Comma) {
                self.digest_symbol(Symbol::RightParentheses)?;
                break;
            }
        }
        self.digest_symbol(Symbol::Arrow)?;
        let return_type = self.parse_type()?.into();
        Ok(Type::Function {
            return_type,
            parameter_types: parameter_types.into(),
        })
    }

    fn parse_type(&mut self) -> Result<Type, CompileError> {
        if self.expect_symbol(Symbol::LeftParentheses) {
            self.parse_function_type()
        } else {
            let type_str = self.parse_plain_identifier()?;
            Type::try_from(type_str)
        }
    }

    fn parse_function_return_type(&mut self) -> Result<Type, CompileError> {
        if self.expect_symbol(Symbol::Arrow) {
            self.parse_type()
        } else {
            Ok(Type::Never)
        }
    }

    fn parse_function_prototype(&mut self) -> Result<FunctionPrototype, CompileError> {
        let identifier = self.parse_plain_identifier()?.into();
        let (parameters, parameter_types) = self.parse_function_parameters()?;
        let return_type = self.parse_function_return_type()?.into();
        Ok(FunctionPrototype {
            identifier,
            parameters,
            function_type: Type::Function {
                return_type,
                parameter_types,
            }
            .into(),
        })
    }

    fn parse_define_statement(
        &mut self,
        context: &mut Context,
    ) -> Result<DefineDetail, CompileError> {
        self.digest_keyword(Keyword::Define)?;
        let builtin = self.expect_keyword(Keyword::Builtin);
        let prototype = self.parse_function_prototype()?;
        let identifier = &prototype.identifier;
        if self.scope.is_global()? {
            let function_type = prototype.function_type.clone();
            self.type_map.insert(identifier.clone(), function_type);
            self.mutability_map
                .insert(identifier.clone(), Mutability::Immutable);
        }
        self.scope.enter(Tag::Function(identifier.clone()));
        let body = self.parse_block_statement(context)?.into();
        self.scope.leave(Tag::Function(identifier.clone()))?;
        Ok(DefineDetail {
            prototype: prototype.into(),
            builtin,
            body,
        })
    }

    fn parse_let_statement(&mut self) -> Result<LetDetail, CompileError> {
        self.digest_keyword(Keyword::Let)?;
        let mutability = if self.expect_keyword(Keyword::Mut) {
            Mutability::Mutable
        } else {
            Mutability::Immutable
        };
        let lvalue = Rc::new(self.parse_plain_identifier()?);
        let lvalue_type = if self.expect_keyword(Keyword::As) {
            self.parse_type()?
        } else {
            Type::Unknown
        };
        let lvalue_type = Rc::new(lvalue_type);
        self.digest_symbol(Symbol::Assign)?;
        let expression = self.parse_expression()?;
        self.digest_symbol(Symbol::Semicolon)?;
        if self.scope.is_global()? {
            self.type_map.insert(lvalue.clone(), lvalue_type.clone());
            self.mutability_map.insert(lvalue.clone(), mutability);
        }
        Ok(LetDetail(
            Variable(lvalue, lvalue_type, mutability),
            expression.into(),
        ))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, CompileError> {
        self.digest_keyword(Keyword::Return)?;
        if self.expect_symbol(Symbol::Semicolon) {
            return Ok(Statement::Return(None));
        }
        let expression = self.parse_expression()?;
        self.digest_symbol(Symbol::Semicolon)?;
        Ok(Statement::Return(Some(expression.into())))
    }

    fn parse_using_statement(&mut self, context: &mut Context) -> Result<Statement, CompileError> {
        self.digest_keyword(Keyword::Using)?;
        let mut path_nodes = Vec::new();
        path_nodes.push(self.parse_plain_identifier()?);
        while self.expect_symbol(Symbol::DoubleColon) {
            path_nodes.push(self.parse_plain_identifier()?);
        }
        self.digest_symbol(Symbol::Semicolon)?;
        let Some((item, path_nodes)) = path_nodes.split_last() else {
            return Err(CompileError::MissingIdentifier);
        };
        let path_nodes = path_nodes.iter().cloned().map(Rc::new).collect();
        let document_path = Rc::new(DocumentPath(path_nodes));
        load_package_path(context, document_path.clone())?;
        let using_path = UsingPath(document_path, item.clone().into());
        Ok(Statement::Using(using_path))
    }

    fn parse_statement(&mut self, context: &mut Context) -> Result<Statement, CompileError> {
        match self.lexer.peek() {
            Some(Token::Keyword(Keyword::Return)) => self.parse_return_statement(),
            Some(Token::Keyword(Keyword::If)) => {
                Ok(Statement::If(self.parse_if_statement(context)?))
            }
            Some(Token::Keyword(Keyword::While)) => {
                Ok(Statement::While(self.parse_while_statement(context)?))
            }
            Some(Token::Symbol(Symbol::LeftBrace)) => self.parse_block_statement(context),
            Some(Token::Keyword(Keyword::Define)) => {
                Ok(Statement::Define(self.parse_define_statement(context)?))
            }
            Some(Token::Keyword(Keyword::Let)) => Ok(Statement::Let(self.parse_let_statement()?)),
            Some(Token::Symbol(Symbol::Semicolon)) => self.parse_empty_statement(),
            Some(Token::Keyword(Keyword::Using)) => {
                let statement = self.parse_using_statement(context)?;
                Ok(statement)
            }
            Some(_) => self.parse_expression_statement(),
            None => Err(CompileError::UnexpectedParseEOF),
        }
    }

    /// # Errors
    pub fn parse_document(&mut self, context: &mut Context) -> Result<DocumentId, CompileError> {
        let id = next_document_id();
        context.id_map.insert(self.document_path.clone(), id);
        context.path_map.insert(id, self.document_path.clone());

        self.scope.enter(Tag::Global);
        let mut statements = Vec::new();
        while self.lexer.peek().is_some() {
            statements.push(self.parse_statement(context)?.into());
        }
        self.scope.leave(Tag::Global)?;

        let document = Document {
            statements: statements.into(),
        };
        context.document_map.insert(id, document);
        context.type_map.insert(id, mem::take(&mut self.type_map));
        context
            .mutability_map
            .insert(id, mem::take(&mut self.mutability_map));
        Ok(id)
    }
}
