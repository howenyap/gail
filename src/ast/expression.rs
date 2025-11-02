use crate::ast::{ExpressionNode, Node};
use crate::token::Token;
use std::fmt::{self, Display};

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

    pub fn prefix(token: Token<'a>, right: Expression<'a>) -> Self {
        Self::Prefix {
            token,
            operator: token.literal,
            right: Box::new(right),
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
            Expression::Prefix { token, .. } => token,
            Expression::Infix { token, .. } => token,
        }
    }

    pub fn is_ident(&self) -> bool {
        matches!(self, Expression::Ident { .. })
    }
}

impl<'a> Node for Expression<'a> {
    fn token_literal(&self) -> &str {
        match self {
            Expression::Ident { token, .. } => token.literal(),
            Expression::Int { token, .. } => token.literal(),
            Expression::Prefix { token, .. } => token.literal(),
            Expression::Infix { token, .. } => token.literal(),
        }
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
            Expression::Prefix {
                operator, right, ..
            } => write!(f, "({operator}{right})"),
            Expression::Infix {
                left,
                operator,
                right,
                ..
            } => write!(f, "({left} {operator} {right})"),
        }
    }
}
