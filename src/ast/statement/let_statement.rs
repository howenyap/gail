use crate::ast::{Expression, Identifier, Node, StatementNode};
use crate::token::Token;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq)]
pub struct LetStatement<'a> {
    pub token: Token<'a>,
    pub name: Identifier<'a>,
    pub value: Expression<'a>,
}

impl<'a> LetStatement<'a> {
    pub fn new(token: Token<'a>, name: Identifier<'a>, value: Expression<'a>) -> Self {
        Self { token, name, value }
    }
}

impl<'a> Node for LetStatement<'a> {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl<'a> StatementNode for LetStatement<'a> {}

impl<'a> Display for LetStatement<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {} = {};", self.name.value, self.value)
    }
}

