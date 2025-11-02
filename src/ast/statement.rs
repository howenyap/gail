use crate::ast::{Expression, Identifier, Node, StatementNode};
use crate::token::Token;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Let {
        token: Token<'a>,
        name: Identifier<'a>,
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
    pub fn r#let(token: Token<'a>, name: Identifier<'a>, value: Expression<'a>) -> Self {
        Self::Let { token, name, value }
    }

    pub fn r#return(token: Token<'a>, value: Expression<'a>) -> Self {
        Self::Return { token, value }
    }

    pub fn expression(token: Token<'a>, expression: Expression<'a>) -> Self {
        Self::Expression { token, expression }
    }
}

impl<'a> Node for Statement<'a> {
    fn token_literal(&self) -> &str {
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

impl<'a> StatementNode for Statement<'a> {
    fn statement_node(&self) {}
}
