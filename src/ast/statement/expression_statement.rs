use crate::ast::{Expression, Node, StatementNode};
use crate::token::Token;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement<'a> {
    pub token: Token<'a>,
    pub expression: Expression<'a>,
}

impl<'a> ExpressionStatement<'a> {
    pub fn new(token: Token<'a>, expression: Expression<'a>) -> Self {
        Self { token, expression }
    }
}

impl<'a> Node for ExpressionStatement<'a> {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl<'a> StatementNode for ExpressionStatement<'a> {}

impl<'a> Display for ExpressionStatement<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expression)
    }
}
