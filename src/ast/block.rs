use crate::ast::Node;
use crate::token::Token;
use std::fmt::{self, Display};

use super::Statement;

#[derive(Debug, PartialEq)]
pub struct Block<'a> {
    token: Token<'a>,
    statements: Vec<Statement<'a>>,
}

impl<'a> Block<'a> {
    pub fn new(token: Token<'a>, statements: Vec<Statement<'a>>) -> Self {
        Self { token, statements }
    }

    pub fn statements(&self) -> &Vec<Statement<'a>> {
        &self.statements
    }
}

impl<'a> Node for Block<'a> {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl<'a> Display for Block<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s: String = self.statements.iter().map(|s| s.to_string()).collect();
        write!(f, "{s}")
    }
}
