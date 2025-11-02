use crate::ast::Node;
use super::{LetStatement, ReturnStatement, ExpressionStatement};
use std::fmt::{self, Display};

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Let(LetStatement<'a>),
    Return(ReturnStatement<'a>),
    Expression(ExpressionStatement<'a>),
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

