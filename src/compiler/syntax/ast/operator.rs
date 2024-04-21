use std::fmt::{self, Display};

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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Unary {
    LogicalNot,
    BitNot,
    Negative,
}

impl Operator for Unary {
    fn precedence(&self) -> u8 {
        match self {
            Unary::LogicalNot | Unary::BitNot | Unary::Negative => 14u8,
        }
    }

    fn is_left_associative(&self) -> bool {
        match self {
            Unary::LogicalNot | Unary::BitNot | Unary::Negative => false,
        }
    }
}

impl Display for Unary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Unary::LogicalNot => "LogicalNot",
            Unary::BitNot => "BitNot",
            Unary::Negative => "Negative",
        };
        write!(f, "{s}")
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Binary {
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

    Assign,
}

impl Operator for Binary {
    fn precedence(&self) -> u8 {
        match self {
            Binary::Multiply | Binary::Divide => 13u8,
            Binary::Add | Binary::Subtract => 12u8,
            Binary::LessThan
            | Binary::LessThanEqual
            | Binary::GreaterThan
            | Binary::GreaterThanEqual => 10u8,
            Binary::Equal | Binary::NotEqual => 9u8,
            Binary::BitAnd => 8u8,
            Binary::BitOr => 6u8,
            Binary::LogicalAnd => 5u8,
            Binary::LogicalOr => 4u8,
            Binary::Assign => 2u8,
        }
    }

    fn is_left_associative(&self) -> bool {
        match self {
            Binary::Add
            | Binary::Subtract
            | Binary::Multiply
            | Binary::Divide
            | Binary::LogicalAnd
            | Binary::LogicalOr
            | Binary::BitAnd
            | Binary::BitOr
            | Binary::Equal
            | Binary::NotEqual
            | Binary::LessThan
            | Binary::LessThanEqual
            | Binary::GreaterThan
            | Binary::GreaterThanEqual => true,
            Binary::Assign => false,
        }
    }
}

impl Display for Binary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Binary::Add => "Add",
            Binary::Subtract => "Subtract",
            Binary::Multiply => "Multiply",
            Binary::Divide => "Divide",
            Binary::LogicalAnd => "LogicalAnd",
            Binary::LogicalOr => "LogicalOr",
            Binary::BitAnd => "BitAnd",
            Binary::BitOr => "BitOr",
            Binary::Equal => "Equal",
            Binary::NotEqual => "NotEqual",
            Binary::LessThan => "LessThan",
            Binary::LessThanEqual => "LessThanEqual",
            Binary::GreaterThan => "GreaterThan",
            Binary::GreaterThanEqual => "GreaterThanEqual",
            Binary::Assign => "Assign",
        };
        write!(f, "{s}")
    }
}
