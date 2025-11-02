use crate::ast::{ExpressionNode, Node};
use crate::token::Token;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq)]
pub struct IntegerLiteral<'a> {
    pub token: Token<'a>,
    pub value: i64,
}

impl<'a> IntegerLiteral<'a> {
    pub fn new(token: Token<'a>, value: i64) -> Self {
        Self { token, value }
    }
}

impl<'a> Node for IntegerLiteral<'a> {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl<'a> ExpressionNode for IntegerLiteral<'a> {
    fn expression_node(&self) {}
}

impl<'a> Display for IntegerLiteral<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token.literal())
    }
}
