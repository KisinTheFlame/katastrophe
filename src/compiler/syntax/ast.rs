pub trait Operator {
    /// (unary) - ! ~ : 14 right
    /// * / : 13 left
    /// + - : 12 left
    /// < > <= >= : 10 left
    /// == != : 9 left
    /// & : 8 left
    /// ^ : 7 left
    /// | : 6 left
    /// && : 5 left
    /// || : 4 left
    /// (assign) = : 2 right
    fn precedence(&self) -> u8;
    fn is_left_associative(&self) -> bool;
}

#[derive(Debug)]
pub enum UnaryOperator {
    LogicalNot,
    BitNot,
    Negative,
}

impl Operator for UnaryOperator {
    fn precedence(&self) -> u8 {
        match self {
            UnaryOperator::LogicalNot | UnaryOperator::BitNot | UnaryOperator::Negative => 14u8,
        }
    }

    fn is_left_associative(&self) -> bool {
        match self {
            UnaryOperator::LogicalNot | UnaryOperator::BitNot | UnaryOperator::Negative => false,
        }
    }
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,

    LogicalAnd,
    LogicalOr,

    BitAnd,
    BitOr,

    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

impl Operator for BinaryOperator {
    fn precedence(&self) -> u8 {
        match self {
            BinaryOperator::Multiply | BinaryOperator::Divide => 13u8,
            BinaryOperator::Add | BinaryOperator::Subtract => 12u8,
            BinaryOperator::LessThan
            | BinaryOperator::LessThanEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterThanEqual => 10u8,
            BinaryOperator::Equal | BinaryOperator::NotEqual => 9u8,
            BinaryOperator::BitAnd => 8u8,
            BinaryOperator::BitOr => 6u8,
            BinaryOperator::LogicalAnd => 5u8,
            BinaryOperator::LogicalOr => 4u8,
        }
    }

    fn is_left_associative(&self) -> bool {
        match self {
            BinaryOperator::Add
            | BinaryOperator::Subtract
            | BinaryOperator::Multiply
            | BinaryOperator::Divide
            | BinaryOperator::LogicalAnd
            | BinaryOperator::LogicalOr
            | BinaryOperator::BitAnd
            | BinaryOperator::BitOr
            | BinaryOperator::Equal
            | BinaryOperator::NotEqual
            | BinaryOperator::LessThan
            | BinaryOperator::LessThanEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterThanEqual => true,
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
pub struct FunctionPrototype {
    pub identifier: String,
    pub parameters: Vec<Parameter>,
}

#[derive(Debug)]
pub enum Statement {
    Empty,
    Block(Vec<Statement>),
    Expression(Expression),
    If {
        condition: Expression,
        body: Box<Statement>,
        else_body: Option<Box<Statement>>,
    },
    Let(String, Expression),
    Define {
        prototype: FunctionPrototype,
        body: Box<Statement>,
    },
}

#[derive(Debug)]
pub struct Parameter {
    pub identifier: String,
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}
