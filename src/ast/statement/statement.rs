use super::{ExpressionStatement, LetStatement, ReturnStatement};
use crate::ast::{Expression, Identifier, Node};
use crate::token::Token;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Let(LetStatement<'a>),
    Return(ReturnStatement<'a>),
    Expression(ExpressionStatement<'a>),
}

impl<'a> Statement<'a> {
    pub fn r#let(token: Token<'a>, name: Identifier<'a>, value: Expression<'a>) -> Self {
        Self::Let(LetStatement::new(token, name, value))
    }

    pub fn r#return(token: Token<'a>, value: Expression<'a>) -> Self {
        Self::Return(ReturnStatement::new(token, value))
    }

    pub fn expression(token: Token<'a>, expression: Expression<'a>) -> Self {
        Self::Expression(ExpressionStatement::new(token, expression))
    }
}

impl<'a> Node for Statement<'a> {
    fn token_literal(&self) -> &str {
        match self {
            Statement::Let(stmt) => stmt.token_literal(),
            Statement::Return(stmt) => stmt.token_literal(),
            Statement::Expression(stmt) => stmt.token_literal(),
        }
    }
}

impl<'a> Display for Statement<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(stmt) => write!(f, "{stmt}"),
            Statement::Return(stmt) => write!(f, "{stmt}"),
            Statement::Expression(stmt) => write!(f, "{stmt}"),
        }
    }
}
