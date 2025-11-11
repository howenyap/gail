use crate::ast::Expression;
use crate::token::Token;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Let {
        token: Token<'a>,
        // invariant: expression must be Identifier
        name: Expression<'a>,
        value: Expression<'a>,
    },
    Return {
        token: Token<'a>,
        value: Expression<'a>,
    },
    Expression {
        token: Token<'a>,
        expression: Expression<'a>,
    },
}

impl<'a> Statement<'a> {
    pub fn r#let(token: Token<'a>, name: Expression<'a>, value: Expression<'a>) -> Self {
        Self::Let { token, name, value }
    }

    pub fn r#return(token: Token<'a>, value: Expression<'a>) -> Self {
        Self::Return { token, value }
    }

    pub fn expression(token: Token<'a>, expression: Expression<'a>) -> Self {
        Self::Expression { token, expression }
    }

    pub fn token_literal(&self) -> &str {
        match self {
            Statement::Let { token, .. } => token.literal(),
            Statement::Return { token, .. } => token.literal(),
            Statement::Expression { token, .. } => token.literal(),
        }
    }
}

impl<'a> Display for Statement<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let { name, value, .. } => write!(f, "let {name} = {value};"),
            Statement::Return { value, .. } => write!(f, "return {value};"),
            Statement::Expression { expression, .. } => write!(f, "{expression}"),
        }
    }
}
