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

    // fn expect_keyword(&mut self, expected_keyword: Keyword) -> bool {
    //     if let Some(Token::Keyword(keyword)) = self.lexer.peek() {
    //         if *keyword == expected_keyword {
    //             return true;
    //         }
    //     }
    //     return false;
    // }

    fn digest_keyword(&mut self, expected_keyword: Keyword) -> Result<(), ParseError> {
        if let Some(Token::Keyword(keyword)) = self.lexer.peek() {
            if *keyword == expected_keyword {
                self.lexer.next();
                return Ok(());
            }
        }
        Err(ParseError {
            kind: ParseErrorKind::MissingKeyword(expected_keyword),
        })
    }

    fn expect_symbol(&mut self, expected_symbol: Symbol) -> bool {
        if let Some(Token::Symbol(symbol)) = self.lexer.peek() {
            if *symbol == expected_symbol {
                return true;
            }
        }
        return false;
    }

    fn digest_symbol(&mut self, expected_symbol: Symbol) -> Result<(), ParseError> {
        if let Some(Token::Symbol(symbol)) = self.lexer.peek() {
            if *symbol == expected_symbol {
                self.lexer.next();
                return Ok(());
            }
        }
        Err(ParseError {
            kind: ParseErrorKind::MissingSymbol(expected_symbol),
        })
    }

    fn parse_function_call_args(&mut self) -> Result<Vec<Expression>, ParseError> {
        self.digest_symbol(Symbol::LeftParentheses)?;
        if self.expect_symbol(Symbol::RightParentheses) {
            self.digest_symbol(Symbol::RightParentheses)?;
            return Ok(Vec::new());
        }
        let mut args = Vec::<Expression>::new();
        loop {
            let arg = self.parse_expression()?;
            args.push(arg);
            if !self.expect_symbol(Symbol::Comma) {
                break;
            }
            self.digest_symbol(Symbol::Comma)?;
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
        if self.expect_symbol(Symbol::LeftParentheses) {
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

    fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        match self.lexer.peek() {
            Some(Token::Identifier(_)) => Ok(self.parse_identifier()?),
            Some(Token::IntLiteral(_)) => Ok(self.parse_integer_literal()?),
            Some(Token::FloatLiteral(_)) => Ok(self.parse_float_literal()?),
            Some(Token::Symbol(Symbol::LeftParentheses)) => {
                Ok(self.parse_parentheses_expression()?)
            }
            Some(Token::Symbol(Symbol::Subtract)) => {
                self.lexer.next();
                Ok(Expression::Unary(
                    UnaryOperator::Negative,
                    Box::new(self.parse_primary()?),
                ))
            }
            Some(token) => Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(token.clone()),
            }),
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
        match symbol {
            Symbol::Add => Ok(Some(BinaryOperator::Add)),
            Symbol::Subtract => Ok(Some(BinaryOperator::Subtract)),
            Symbol::Multiply => Ok(Some(BinaryOperator::Multiply)),
            Symbol::Divide => Ok(Some(BinaryOperator::Divide)),
            _ => Ok(None),
        }
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
        return Ok(expression);
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
        self.digest_symbol(Symbol::RightBrace)?;
        Ok(Statement::Block(statements))
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.lexer.peek() {
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
            self.digest_symbol(Symbol::RightParentheses)?;
            return Ok(Vec::new());
        }
        let mut parameters = Vec::<Parameter>::new();
        loop {
            let parameter = self.parse_function_parameter()?;
            parameters.push(parameter);
            if !self.expect_symbol(Symbol::Comma) {
                break;
            }
            self.digest_symbol(Symbol::Comma)?;
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
