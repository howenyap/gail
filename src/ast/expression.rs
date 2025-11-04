use crate::ast::{ExpressionNode, Node};
use crate::token::Token;
use std::fmt::{self, Display};

use super::block::Block;

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    Ident {
        token: Token<'a>,
        value: &'a str,
    },
    Int {
        token: Token<'a>,
        value: i64,
    },
    Bool {
        token: Token<'a>,
        value: bool,
    },
    If {
        token: Token<'a>,
        condition: Box<Expression<'a>>,
        consequence: Block<'a>,
        alternative: Option<Block<'a>>,
    },
    Prefix {
        token: Token<'a>,
        operator: &'a str,
        right: Box<Expression<'a>>,
    },
    Infix {
        token: Token<'a>,
        left: Box<Expression<'a>>,
        operator: &'a str,
        right: Box<Expression<'a>>,
    },
}

impl<'a> Expression<'a> {
    pub fn ident(token: Token<'a>) -> Self {
        Self::Ident {
            token,
            value: token.literal,
        }
    }

    pub fn int(token: Token<'a>, value: i64) -> Self {
        Self::Int { token, value }
    }

    pub fn bool(token: Token<'a>, value: bool) -> Self {
        Self::Bool { token, value }
    }

    pub fn prefix(token: Token<'a>, right: Expression<'a>) -> Self {
        Self::Prefix {
            token,
            operator: token.literal,
            right: Box::new(right),
        }
    }

    pub fn r#if(
        token: Token<'a>,
        condition: Expression<'a>,
        consequence: Block<'a>,
        alternative: Option<Block<'a>>,
    ) -> Self {
        Self::If {
            token,
            condition: Box::new(condition),
            consequence,
            alternative,
        }
    }

    pub fn infix(token: Token<'a>, left: Expression<'a>, right: Expression<'a>) -> Self {
        Self::Infix {
            token,
            left: Box::new(left),
            operator: token.literal,
            right: Box::new(right),
        }
    }

    pub fn token(&self) -> &Token<'a> {
        match self {
            Expression::Ident { token, .. } => token,
            Expression::Int { token, .. } => token,
            Expression::Bool { token, .. } => token,
            Expression::Prefix { token, .. } => token,
            Expression::Infix { token, .. } => token,
            Expression::If { token, .. } => token,
        }
    }

    pub fn is_ident(&self) -> bool {
        matches!(self, Expression::Ident { .. })
    }
}

impl<'a> Node for Expression<'a> {
    fn token_literal(&self) -> &str {
        self.token().literal()
    }
}

impl<'a> ExpressionNode for Expression<'a> {
    fn expression_node(&self) {}
}

impl<'a> Display for Expression<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Ident { value, .. } => write!(f, "{value}"),
            Expression::Int { token, .. } => write!(f, "{}", token.literal()),
            Expression::Bool { value, .. } => write!(f, "{value}"),
            Expression::Prefix {
                operator, right, ..
            } => write!(f, "({operator}{right})"),
            Expression::Infix {
                left,
                operator,
                right,
                ..
            } => write!(f, "({left} {operator} {right})"),
            Expression::If {
                condition,
                consequence,
                alternative,
                ..
            } => {
                if let Some(alternative) = alternative {
                    write!(f, "if {condition} {consequence} else {alternative}")
                } else {
                    write!(f, "if {condition} {consequence}")
                }
            }
        }
    }
}
