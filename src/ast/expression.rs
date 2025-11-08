use crate::ast::{ExpressionNode, Node};
use crate::token::Token;
use std::fmt::{self, Display};

use super::Statement;

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
        // invariant: expression must be Block
        consequence: Box<Expression<'a>>,
        // invariant: expression must be Block
        alternative: Box<Option<Expression<'a>>>,
    },
    Function {
        token: Token<'a>,
        // invariant: expression must be Identifier
        parameters: Vec<Expression<'a>>,
        // invariant: expression must be Block
        body: Box<Expression<'a>>,
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
    Block {
        token: Token<'a>,
        statements: Vec<Statement<'a>>,
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
        consequence: Expression<'a>,
        alternative: Option<Expression<'a>>,
    ) -> Self {
        Self::If {
            token,
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative: Box::new(alternative),
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

    pub fn block(token: Token<'a>, statements: Vec<Statement<'a>>) -> Self {
        Self::Block { token, statements }
    }

    pub fn function(
        token: Token<'a>,
        parameters: Vec<Expression<'a>>,
        body: Expression<'a>,
    ) -> Self {
        Self::Function {
            token,
            parameters,
            body: Box::new(body),
        }
    }

    pub fn token(&self) -> &Token<'a> {
        match self {
            Self::Ident { token, .. } => token,
            Self::Int { token, .. } => token,
            Self::Bool { token, .. } => token,
            Self::Prefix { token, .. } => token,
            Self::Infix { token, .. } => token,
            Self::If { token, .. } => token,
            Self::Function { token, .. } => token,
            Self::Block { token, .. } => token,
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
                if let Some(alternative) = alternative.as_ref() {
                    write!(f, "if {condition} {consequence} else {alternative}")
                } else {
                    write!(f, "if {condition} {consequence}")
                }
            }
            Expression::Function {
                parameters, body, ..
            } => {
                let params: String = parameters
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "fn({params}) {body} ")
            }
            Expression::Block { statements, .. } => {
                let s: String = statements.iter().map(|s| s.to_string()).collect();
                write!(f, "{{{s}}}")
            }
        }
    }
}
