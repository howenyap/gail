use crate::token::Token;
use std::fmt::{self, Display};

use super::Statement;

#[derive(Debug, PartialEq, Clone)]
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
    Call {
        token: Token<'a>,
        // invariant: expression must be Function
        function: Box<Expression<'a>>,
        arguments: Vec<Expression<'a>>,
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

    pub fn call(
        token: Token<'a>,
        function: Expression<'a>,
        arguments: Vec<Expression<'a>>,
    ) -> Self {
        Self::Call {
            token,
            function: Box::new(function),
            arguments,
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
            Self::Call { token, .. } => token,
        }
    }

    pub fn token_literal(&self) -> &str {
        self.token().literal()
    }
}

impl<'a> Display for Expression<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident { value, .. } => write!(f, "{value}"),
            Self::Int { value, .. } => write!(f, "{value}"),
            Self::Bool { value, .. } => write!(f, "{value}"),
            Self::Prefix {
                operator, right, ..
            } => write!(f, "({operator}{right})"),
            Self::Infix {
                left,
                operator,
                right,
                ..
            } => write!(f, "({left} {operator} {right})"),
            Self::If {
                condition,
                consequence,
                alternative,
                ..
            } => match alternative.as_ref() {
                Some(alternative) => write!(f, "if {condition} {consequence} else {alternative}"),
                None => write!(f, "if {condition} {consequence}"),
            },
            Self::Function {
                parameters, body, ..
            } => {
                let params: String = parameters
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "fn({params}) {body} ")
            }
            Self::Block { statements, .. } => {
                let s: String = statements.iter().map(|s| s.to_string()).collect();
                write!(f, "{s}")
            }
            Self::Call {
                function,
                arguments,
                ..
            } => {
                let args: String = arguments
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "{function}({args})")
            }
        }
    }
}
