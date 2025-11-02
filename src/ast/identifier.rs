use crate::ast::Node;
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

impl<'a> Node for Identifier<'a> {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl<'a> Display for Identifier<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
