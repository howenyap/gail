use crate::ast::{Expression, Node, StatementNode};
use crate::token::Token;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq)]
pub struct ReturnStatement<'a> {
    pub token: Token<'a>,
    pub value: Expression<'a>,
}

impl<'a> ReturnStatement<'a> {
    pub fn new(token: Token<'a>, value: Expression<'a>) -> Self {
        Self { token, value }
    }
}

impl<'a> Node for ReturnStatement<'a> {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl<'a> StatementNode for ReturnStatement<'a> {}

impl<'a> Display for ReturnStatement<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "return {};", self.value)
    }
}

