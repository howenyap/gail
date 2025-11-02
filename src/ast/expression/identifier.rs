use crate::ast::{ExpressionNode, Node};
use crate::token::Token;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq)]
pub struct Identifier<'a> {
    pub token: Token<'a>,
    pub value: &'a str,
}

impl<'a> Identifier<'a> {
    pub fn new(token: Token<'a>) -> Self {
        Self {
            token,
            value: token.literal,
        }
    }
}

impl Node for Identifier<'_> {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl ExpressionNode for Identifier<'_> {
    fn expression_node(&self) {}
}

impl Display for Identifier<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
