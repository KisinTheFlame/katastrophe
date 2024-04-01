use super::{
    ast::{
        BinaryOperator, Expression, Function, FunctionPrototype, Operator, Parameter, Program,
        Statement, UnaryOperator,
    },
    error::{ParseError, ParseErrorKind},
    lexer::Lexer,
    token::{Keyword, Symbol, Token},
};

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(code: String) -> Parser {
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
        return false;
    }

    fn expect_keyword(&mut self, expected_keyword: Keyword) -> bool {
        let matched = self.match_keyword(expected_keyword);
        if matched {
            self.lexer.next();
        }
        matched
    }

    fn digest_keyword(&mut self, expected_keyword: Keyword) -> Result<(), ParseError> {
        if self.expect_keyword(expected_keyword.clone()) {
            Ok(())
        } else {
            Err(ParseError {
                kind: ParseErrorKind::MissingKeyword(expected_keyword),
            })
        }
    }

    fn match_symbol(&mut self, expected_symbol: Symbol) -> bool {
        if let Some(Token::Symbol(symbol)) = self.lexer.peek() {
            if *symbol == expected_symbol {
                return true;
            }
        }
        return false;
    }

    fn expect_symbol(&mut self, expected_symbol: Symbol) -> bool {
        let matched = self.match_symbol(expected_symbol);
        if matched {
            self.lexer.next();
        }
        matched
    }

    fn digest_symbol(&mut self, expected_symbol: Symbol) -> Result<(), ParseError> {
        if self.expect_symbol(expected_symbol.clone()) {
            Ok(())
        } else {
            Err(ParseError {
                kind: ParseErrorKind::MissingSymbol(expected_symbol),
            })
        }
    }

    fn parse_function_call_args(&mut self) -> Result<Vec<Expression>, ParseError> {
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

    fn parse_plain_identifier(&mut self) -> Result<String, ParseError> {
        if let Some(Token::Identifier(identifier)) = self.lexer.peek() {
            let identifier = identifier.clone();
            self.lexer.next();
            Ok(identifier)
        } else {
            Err(ParseError {
                kind: ParseErrorKind::MissingIdentifier,
            })
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression, ParseError> {
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

    fn parse_integer_literal(&mut self) -> Result<Expression, ParseError> {
        if let Some(Token::IntLiteral(integer)) = self.lexer.peek() {
            let integer = *integer;
            self.lexer.next();
            Ok(Expression::IntLiteral(integer))
        } else {
            Err(ParseError {
                kind: ParseErrorKind::InternalMess("should be an integer"),
            })
        }
    }

    fn parse_float_literal(&mut self) -> Result<Expression, ParseError> {
        if let Some(Token::FloatLiteral(float)) = self.lexer.peek() {
            let float = *float;
            self.lexer.next();
            Ok(Expression::FloatLiteral(float))
        } else {
            Err(ParseError {
                kind: ParseErrorKind::InternalMess("should be an integer"),
            })
        }
    }

    fn parse_unary_expression(&mut self) -> Result<Expression, ParseError> {
        if let Some(Token::Symbol(symbol)) = self.lexer.peek() {
            let operator = match symbol {
                Symbol::Subtract => UnaryOperator::Negative,
                Symbol::LogicalNot => UnaryOperator::LogicalNot,
                Symbol::BitNot => UnaryOperator::BitNot,
                symbol => {
                    return Err(ParseError {
                        kind: ParseErrorKind::MissingSymbol(symbol.clone()),
                    });
                }
            };
            self.lexer.next();
            Ok(Expression::Unary(operator, Box::new(self.parse_primary()?)))
        } else {
            let kind = if let Some(token) = self.lexer.peek() {
                ParseErrorKind::UnexpectedToken(token.clone())
            } else {
                ParseErrorKind::UnexpectedEOF
            };
            Err(ParseError { kind })
        }
    }

    fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        match self.lexer.peek() {
            Some(Token::Identifier(_)) => Ok(self.parse_identifier()?),
            Some(Token::IntLiteral(_)) => Ok(self.parse_integer_literal()?),
            Some(Token::FloatLiteral(_)) => Ok(self.parse_float_literal()?),
            Some(Token::Symbol(Symbol::LeftParentheses)) => {
                Ok(self.parse_parentheses_expression()?)
            }
            Some(_) => Ok(self.parse_unary_expression()?),
            None => Err(ParseError {
                kind: ParseErrorKind::UnexpectedEOF,
            }),
        }
    }

    fn peek_binary_operator(&mut self) -> Result<Option<BinaryOperator>, ParseError> {
        let symbol = match self.lexer.peek() {
            Some(Token::Symbol(symbol)) => symbol.clone(),
            Some(_) | None => {
                return Ok(None);
            }
        };
        let symbol = match symbol {
            Symbol::Add => Some(BinaryOperator::Add),
            Symbol::Subtract => Some(BinaryOperator::Subtract),
            Symbol::Multiply => Some(BinaryOperator::Multiply),
            Symbol::Divide => Some(BinaryOperator::Divide),
            Symbol::LogicalAnd => Some(BinaryOperator::LogicalAnd),
            Symbol::LogicalOr => Some(BinaryOperator::LogicalOr),
            Symbol::BitAnd => Some(BinaryOperator::BitAnd),
            Symbol::BitOr => Some(BinaryOperator::BitOr),
            Symbol::Equal => Some(BinaryOperator::Equal),
            Symbol::NotEqual => Some(BinaryOperator::NotEqual),
            Symbol::LessThan => Some(BinaryOperator::LessThan),
            Symbol::LessThanEqual => Some(BinaryOperator::LessThanEqual),
            Symbol::GreaterThan => Some(BinaryOperator::GreaterThan),
            Symbol::GreaterThanEqual => Some(BinaryOperator::GreaterThanEqual),
            _ => None,
        };
        Ok(symbol)
    }

    fn parse_binary_expression_rhs(
        &mut self,
        last_precedence: u8,
        lhs: Expression,
    ) -> Result<Expression, ParseError> {
        let mut lhs = lhs;
        loop {
            let current_operator = match self.peek_binary_operator()? {
                Some(operator) => operator,
                None => {
                    return Ok(lhs);
                }
            };

            if current_operator.precedence() < last_precedence {
                return Ok(lhs);
            }

            self.lexer.next();

            let rhs = self.parse_primary()?;

            let next_operator = match self.peek_binary_operator()? {
                Some(operator) => operator,
                None => {
                    return Ok(Expression::Binary(
                        current_operator,
                        Box::new(lhs),
                        Box::new(rhs),
                    ));
                }
            };
            let rhs = if next_operator.precedence() > current_operator.precedence() {
                self.parse_binary_expression_rhs(next_operator.precedence(), rhs)?
            } else {
                rhs
            };

            lhs = Expression::Binary(current_operator, Box::new(lhs), Box::new(rhs));
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        let lhs = self.parse_primary()?;

        self.parse_binary_expression_rhs(0, lhs)
    }

    fn parse_parentheses_expression(&mut self) -> Result<Expression, ParseError> {
        self.digest_symbol(Symbol::LeftParentheses)?;
        let expression = self.parse_expression()?;
        self.digest_symbol(Symbol::RightParentheses)?;
        Ok(expression)
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expression = self.parse_expression()?;
        self.digest_symbol(Symbol::Semicolon)?;
        Ok(Statement::Expression(expression))
    }

    fn parse_block_statement(&mut self) -> Result<Statement, ParseError> {
        let mut statements = Vec::<Statement>::new();
        self.digest_symbol(Symbol::LeftBrace)?;
        while !self.expect_symbol(Symbol::RightBrace) {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }
        Ok(Statement::Block(statements))
    }

    fn parse_if_statement(&mut self) -> Result<Statement, ParseError> {
        self.digest_keyword(Keyword::If)?;
        let condition = self.parse_expression()?;
        let body = Box::new(self.parse_block_statement()?);
        let else_body = if self.expect_keyword(Keyword::Else) {
            Some(Box::new(self.parse_block_statement()?))
        } else {
            None
        };
        Ok(Statement::If {
            condition,
            body,
            else_body,
        })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.lexer.peek() {
            Some(Token::Keyword(Keyword::If)) => self.parse_if_statement(),
            Some(Token::Symbol(Symbol::LeftBrace)) => self.parse_block_statement(),
            Some(_) => self.parse_expression_statement(),
            None => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedEOF,
                });
            }
        }
    }

    fn parse_function_parameter(&mut self) -> Result<Parameter, ParseError> {
        let identifier = self.parse_plain_identifier()?;
        Ok(Parameter { identifier })
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Parameter>, ParseError> {
        self.digest_symbol(Symbol::LeftParentheses)?;
        if self.expect_symbol(Symbol::RightParentheses) {
            return Ok(Vec::new());
        }
        let mut parameters = Vec::<Parameter>::new();
        loop {
            let parameter = self.parse_function_parameter()?;
            parameters.push(parameter);
            if !self.expect_symbol(Symbol::Comma) {
                break;
            }
        }
        self.digest_symbol(Symbol::RightParentheses)?;
        Ok(parameters)
    }

    fn parse_function_prototype(&mut self) -> Result<FunctionPrototype, ParseError> {
        let identifier = self.parse_plain_identifier()?;
        Ok(FunctionPrototype {
            identifier,
            parameters: self.parse_function_parameters()?,
        })
    }

    fn parse_function(&mut self) -> Result<Function, ParseError> {
        self.digest_keyword(Keyword::Define)?;
        let prototype = self.parse_function_prototype()?;
        let body = self.parse_block_statement()?;
        Ok(Function { prototype, body })
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut functions = Vec::<Function>::new();
        while self.lexer.peek() != None {
            functions.push(self.parse_function()?);
        }
        Ok(Program { functions })
    }
}
