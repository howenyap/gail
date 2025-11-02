use super::Expression;
use crate::ast::{ExpressionNode, Node};
use crate::token::Token;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq)]
pub struct PrefixExpression<'a> {
    pub token: Token<'a>,
    pub operator: &'a str,
    pub right: Box<Expression<'a>>,
}

impl<'a> PrefixExpression<'a> {
    pub fn new(token: Token<'a>, operator: &'a str, right: Expression<'a>) -> Self {
        Self {
            token,
            operator,
            right: Box::new(right),
        }
    }
}

impl<'a> Node for PrefixExpression<'a> {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl<'a> ExpressionNode for PrefixExpression<'a> {
    fn expression_node(&self) {}
}

impl<'a> Display for PrefixExpression<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}
