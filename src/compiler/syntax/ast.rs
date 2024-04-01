pub trait Operator {
    /// (unary) - : 15 right
    /// * / : 13 left
    /// + - : 12 left
    /// < > <= >= : 10 left
    /// == != : 9 left
    /// (assign) = : 2 right
    fn precedence(&self) -> u8;
    fn is_left_associative(&self) -> bool;
}

#[derive(Debug)]
pub enum UnaryOperator {
    Negative,
}

impl Operator for UnaryOperator {
    fn precedence(&self) -> u8 {
        match self {
            UnaryOperator::Negative => 15u8,
        }
    }

    fn is_left_associative(&self) -> bool {
        match self {
            UnaryOperator::Negative => false,
        }
    }
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl Operator for BinaryOperator {
    fn precedence(&self) -> u8 {
        match self {
            BinaryOperator::Add => 12u8,
            BinaryOperator::Subtract => 12u8,
            BinaryOperator::Multiply => 13u8,
            BinaryOperator::Divide => 13u8,
        }
    }

    fn is_left_associative(&self) -> bool {
        match self {
            BinaryOperator::Add => true,
            BinaryOperator::Subtract => true,
            BinaryOperator::Multiply => true,
            BinaryOperator::Divide => true,
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Identifier(String),

    IntLiteral(i32),
    FloatLiteral(f64),

    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),

    Call(String, Vec<Expression>),
}

#[derive(Debug)]
pub enum Statement {
    Block(Vec<Statement>),
    Expression(Expression),
    If {
        condition: Expression,
        body: Box<Statement>,
        else_body: Option<Box<Statement>>,
    },
}

#[derive(Debug)]
pub struct Parameter {
    pub identifier: String,
}

#[derive(Debug)]
pub struct FunctionPrototype {
    pub identifier: String,
    pub parameters: Vec<Parameter>,
}

#[derive(Debug)]
pub struct Function {
    pub prototype: FunctionPrototype,
    pub body: Statement,
}

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Function>,
}
