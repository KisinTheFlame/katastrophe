use crate::compiler::{
    err::CompileError,
    lexis::{
        lexer::Lexer,
        token::{Keyword, Symbol, Token},
    },
};

use super::{
    ast::{
        crumb::{FunctionPrototype, Mutability, Parameter, Variable},
        expression::Expression,
        operator::{Binary, Operator, Unary},
        statement::{DefineDetail, IfDetail, LetDetail, Statement, WhileDetail},
        ty::Type,
        Program,
    },
    err::{ParseError, ParseErrorKind},
};

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    #[must_use]
    pub fn new(code: &str) -> Parser {
        Parser {
            lexer: Lexer::new(code),
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
            Err(ParseError {
                kind: ParseErrorKind::MissingKeyword(expected_keyword),
            }
            .into())
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
            Err(ParseError {
                kind: ParseErrorKind::MissingSymbol(expected_symbol),
            }
            .into())
        }
    }

    fn parse_function_call_args(&mut self) -> Result<Vec<Expression>, CompileError> {
        self.digest_symbol(Symbol::LeftParentheses)?;
        if self.expect_symbol(Symbol::RightParentheses) {
            return Ok(Vec::new());
        }
        let mut args = Vec::<Expression>::new();
        loop {
            let arg = self.parse_expression()?;
            args.push(arg);
            if !self.expect_symbol(Symbol::Comma) {
                break;
            }
        }
        self.digest_symbol(Symbol::RightParentheses)?;
        Ok(args)
    }

    fn parse_plain_identifier(&mut self) -> Result<String, CompileError> {
        if let Some(Token::Identifier(identifier)) = self.lexer.peek() {
            let identifier = identifier.clone();
            self.lexer.next();
            Ok(identifier)
        } else {
            Err(ParseError {
                kind: ParseErrorKind::MissingIdentifier,
            }
            .into())
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression, CompileError> {
        let identifier = self.parse_plain_identifier()?;
        if self.match_symbol(Symbol::LeftParentheses) {
            Ok(Expression::Call(
                identifier,
                self.parse_function_call_args()?,
            ))
        } else {
            Ok(Expression::Identifier(identifier))
        }
    }

    fn parse_integer_literal(&mut self, literal: i32) -> Expression {
        self.lexer.next();
        Expression::IntLiteral(literal)
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
                    return Err(ParseError {
                        kind: ParseErrorKind::MissingSymbol(*symbol),
                    }
                    .into());
                }
            };
            self.lexer.next();
            Ok(Expression::Unary(
                operator,
                Type::Unknown,
                Box::new(self.parse_primary()?),
            ))
        } else {
            let kind = if let Some(token) = self.lexer.peek() {
                ParseErrorKind::UnexpectedToken(token.clone())
            } else {
                ParseErrorKind::UnexpectedEOF
            };
            Err(ParseError { kind }.into())
        }
    }

    fn parse_primary(&mut self) -> Result<Expression, CompileError> {
        match self.lexer.peek() {
            Some(Token::Identifier(_)) => Ok(self.parse_identifier()?),
            Some(Token::IntLiteral(literal)) => {
                let literal = *literal;
                Ok(self.parse_integer_literal(literal))
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
            None => Err(ParseError {
                kind: ParseErrorKind::UnexpectedEOF,
            }
            .into()),
        }
    }

    fn peek_binary_operator(&mut self) -> Option<Binary> {
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
            | Symbol::Arrow => None,
        }
    }

    fn parse_binary_expression_rhs(
        &mut self,
        last_precedence: u8,
        lhs: Expression,
    ) -> Result<Expression, CompileError> {
        let mut lhs = lhs;
        loop {
            let Some(current_operator) = self.peek_binary_operator() else {
                return Ok(lhs);
            };

            if current_operator.precedence() < last_precedence {
                return Ok(lhs);
            }

            self.lexer.next();

            let rhs = self.parse_primary()?;

            let Some(next_operator) = self.peek_binary_operator() else {
                return Ok(Expression::Binary(
                    current_operator,
                    Type::Unknown,
                    Box::new(lhs),
                    Box::new(rhs),
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
                Type::Unknown,
                Box::new(lhs),
                Box::new(rhs),
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
        Ok(Statement::Expression(expression))
    }

    fn parse_block_statement(&mut self) -> Result<Statement, CompileError> {
        let mut statements = Vec::<Statement>::new();
        self.digest_symbol(Symbol::LeftBrace)?;
        while !self.expect_symbol(Symbol::RightBrace) {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }
        Ok(Statement::Block(statements))
    }

    fn parse_if_statement(&mut self) -> Result<IfDetail, CompileError> {
        self.digest_keyword(Keyword::If)?;
        let condition = self.parse_expression()?;
        let body = Box::new(self.parse_block_statement()?);
        let else_body = if self.expect_keyword(Keyword::Else) {
            Some(Box::new(self.parse_block_statement()?))
        } else {
            None
        };
        Ok(IfDetail {
            condition,
            true_body: body,
            false_body: else_body,
        })
    }

    fn parse_while_statement(&mut self) -> Result<WhileDetail, CompileError> {
        self.digest_keyword(Keyword::While)?;
        let condition = self.parse_expression()?;
        let body = Box::new(self.parse_block_statement()?);
        Ok(WhileDetail(condition, body))
    }

    fn parse_function_parameter(&mut self) -> Result<(Parameter, Type), CompileError> {
        let identifier = self.parse_plain_identifier()?;
        self.digest_keyword(Keyword::As)?;
        let parameter_type = self.parse_type()?;
        Ok((Parameter(identifier), parameter_type))
    }

    fn parse_function_parameters(&mut self) -> Result<(Vec<Parameter>, Vec<Type>), CompileError> {
        self.digest_symbol(Symbol::LeftParentheses)?;
        if self.expect_symbol(Symbol::RightParentheses) {
            return Ok((Vec::new(), Vec::new()));
        }
        let mut parameters = Vec::<Parameter>::new();
        let mut parameter_types = Vec::<Type>::new();
        loop {
            let (parameter, parameter_type) = self.parse_function_parameter()?;
            parameters.push(parameter);
            parameter_types.push(parameter_type);
            if !self.expect_symbol(Symbol::Comma) {
                break;
            }
        }
        self.digest_symbol(Symbol::RightParentheses)?;
        Ok((parameters, parameter_types))
    }

    fn parse_function_type(&mut self) -> Result<Type, CompileError> {
        let mut parameter_types = Vec::new();
        loop {
            if self.expect_symbol(Symbol::RightParentheses) {
                break;
            }
            parameter_types.push(self.parse_type()?);
            if !self.expect_symbol(Symbol::Comma) {
                self.digest_symbol(Symbol::RightParentheses)?;
                break;
            }
        }
        self.digest_symbol(Symbol::Arrow)?;
        let return_type = Box::new(self.parse_type()?);
        Ok(Type::Function {
            return_type,
            parameter_types,
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
        let identifier = self.parse_plain_identifier()?;
        let (parameters, parameter_types) = self.parse_function_parameters()?;
        let return_type = self.parse_function_return_type()?.into();
        Ok(FunctionPrototype {
            identifier,
            parameters,
            function_type: Type::Function {
                return_type,
                parameter_types,
            },
        })
    }

    fn parse_define_statement(&mut self) -> Result<DefineDetail, CompileError> {
        self.digest_keyword(Keyword::Define)?;
        let prototype = self.parse_function_prototype()?;
        let body = Box::new(self.parse_block_statement()?);
        Ok(DefineDetail { prototype, body })
    }

    fn parse_let_statement(&mut self) -> Result<LetDetail, CompileError> {
        self.digest_keyword(Keyword::Let)?;
        let mutability = if self.expect_keyword(Keyword::Mut) {
            Mutability::Mutable
        } else {
            Mutability::Immutable
        };
        let lvalue = self.parse_plain_identifier()?;
        let lvalue_type = if self.expect_keyword(Keyword::As) {
            self.parse_type()?
        } else {
            Type::Unknown
        };
        self.digest_symbol(Symbol::Assign)?;
        let expression = self.parse_expression()?;
        self.digest_symbol(Symbol::Semicolon)?;
        Ok(LetDetail(
            Variable(lvalue, lvalue_type, mutability),
            expression,
        ))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, CompileError> {
        self.digest_keyword(Keyword::Return)?;
        if self.expect_symbol(Symbol::Semicolon) {
            return Ok(Statement::Return(None));
        }
        let expression = self.parse_expression()?;
        self.digest_symbol(Symbol::Semicolon)?;
        Ok(Statement::Return(Some(expression)))
    }

    fn parse_statement(&mut self) -> Result<Statement, CompileError> {
        match self.lexer.peek() {
            Some(Token::Keyword(Keyword::Return)) => self.parse_return_statement(),
            Some(Token::Keyword(Keyword::If)) => Ok(Statement::If(self.parse_if_statement()?)),
            Some(Token::Keyword(Keyword::While)) => {
                Ok(Statement::While(self.parse_while_statement()?))
            }
            Some(Token::Symbol(Symbol::LeftBrace)) => self.parse_block_statement(),
            Some(Token::Keyword(Keyword::Define)) => {
                Ok(Statement::Define(self.parse_define_statement()?))
            }
            Some(Token::Keyword(Keyword::Let)) => Ok(Statement::Let(self.parse_let_statement()?)),
            Some(Token::Symbol(Symbol::Semicolon)) => self.parse_empty_statement(),
            Some(_) => self.parse_expression_statement(),
            None => Err(ParseError {
                kind: ParseErrorKind::UnexpectedEOF,
            }
            .into()),
        }
    }

    /// # Errors
    pub fn parse_program(&mut self) -> Result<Program, CompileError> {
        let mut statements = Vec::<Statement>::new();
        while self.lexer.peek().is_some() {
            statements.push(self.parse_statement()?);
        }
        Ok(Program { statements })
    }
}
