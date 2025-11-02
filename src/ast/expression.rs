use crate::token::Token;
use crate::ast::{Node, ExpressionNode};
use super::identifier::Identifier;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    Identifier(Identifier<'a>),
    Empty,
}

impl<'a> Expression<'a> {
    pub fn new(token: Token<'a>) -> Self {
        Self::Identifier(Identifier::new(token))
    }
}

impl<'a> Node for Expression<'a> {
    fn token_literal(&self) -> &str {
        match self {
            Expression::Identifier(expr) => expr.token_literal(),
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
            Expression::Empty => write!(f, "empty expression"),
        }
    }
}

