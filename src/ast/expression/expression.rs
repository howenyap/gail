use super::{
    identifier::Identifier, infix::InfixExpression, integer_literal::IntegerLiteral,
    prefix::PrefixExpression,
};
use crate::ast::{ExpressionNode, Node};
use crate::token::Token;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    Identifier(Identifier<'a>),
    Integer(IntegerLiteral<'a>),
    Prefix(PrefixExpression<'a>),
    Infix(InfixExpression<'a>),
    Empty,
}

impl<'a> Expression<'a> {
    pub fn new(token: Token<'a>) -> Self {
        Self::Identifier(Identifier::new(token))
    }

    pub fn identifier(token: Token<'a>) -> Self {
        Self::Identifier(Identifier::new(token))
    }

    pub fn integer(token: Token<'a>, value: i64) -> Self {
        Self::Integer(IntegerLiteral::new(token, value))
    }

    pub fn prefix(token: Token<'a>, operator: &'a str, right: Expression<'a>) -> Self {
        Self::Prefix(PrefixExpression::new(token, operator, right))
    }

    pub fn infix(
        token: Token<'a>,
        left: Expression<'a>,
        operator: &'a str,
        right: Expression<'a>,
    ) -> Self {
        Self::Infix(InfixExpression::new(token, left, operator, right))
    }
}

impl<'a> Node for Expression<'a> {
    fn token_literal(&self) -> &str {
        match self {
            Expression::Identifier(expr) => expr.token_literal(),
            Expression::Integer(expr) => expr.token_literal(),
            Expression::Prefix(expr) => expr.token_literal(),
            Expression::Infix(expr) => expr.token_literal(),
            Expression::Empty => "empty expression",
        }
    }
}

impl<'a> ExpressionNode for Expression<'a> {
    fn expression_node(&self) {}
}

impl<'a> Display for Expression<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(expr) => write!(f, "{expr}"),
            Expression::Integer(expr) => write!(f, "{expr}"),
            Expression::Prefix(expr) => write!(f, "{expr}"),
            Expression::Infix(expr) => write!(f, "{expr}"),
            Expression::Empty => write!(f, "empty expression"),
        }
    }
}
