use crate::token::Token;
use std::fmt::{self, Display};

use super::Statement;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Ident {
        value: String,
    },
    Int {
        value: i64,
    },
    Bool {
        value: bool,
    },
    String {
        value: String,
    },
    If {
        condition: Box<Expression>,
        // invariant: expression must be Block
        consequence: Box<Expression>,
        // invariant: expression must be Block
        alternative: Box<Option<Expression>>,
    },
    Function {
        // invariant: expression must be Identifier
        parameters: Vec<Expression>,
        // invariant: expression must be Block
        body: Box<Expression>,
    },
    Prefix {
        operator: String,
        right: Box<Expression>,
    },
    Infix {
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
    Block {
        statements: Vec<Statement>,
    },
    Call {
        // invariant: expression must be Function
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

impl Expression {
    pub fn ident(token: &Token) -> Self {
        Self::Ident {
            value: token.to_string(),
        }
    }

    pub fn int(value: i64) -> Self {
        Self::Int { value }
    }

    pub fn bool(value: &Token) -> Self {
        Self::Bool {
            value: value.to_string() == "true",
        }
    }

    pub fn string(token: &Token) -> Self {
        Self::String {
            value: token.to_string(),
        }
    }

    pub fn prefix(token: &Token, right: Expression) -> Self {
        Self::Prefix {
            operator: token.to_string(),
            right: Box::new(right),
        }
    }

    pub fn r#if(
        condition: Expression,
        consequence: Expression,
        alternative: Option<Expression>,
    ) -> Self {
        Self::If {
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative: Box::new(alternative),
        }
    }

    pub fn infix(token: &Token, left: Expression, right: Expression) -> Self {
        Self::Infix {
            left: Box::new(left),
            operator: token.to_string(),
            right: Box::new(right),
        }
    }

    pub fn block(statements: Vec<Statement>) -> Self {
        Self::Block { statements }
    }

    pub fn function(parameters: Vec<Expression>, body: Expression) -> Self {
        Self::Function {
            parameters,
            body: Box::new(body),
        }
    }

    pub fn call(function: Expression, arguments: Vec<Expression>) -> Self {
        Self::Call {
            function: Box::new(function),
            arguments,
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident { value } => write!(f, "{value}"),
            Self::Int { value } => write!(f, "{value}"),
            Self::Bool { value } => write!(f, "{value}"),
            Self::String { value } => write!(f, "{value}"),
            Self::Prefix { operator, right } => write!(f, "({operator}{right})"),
            Self::Infix {
                left,
                operator,
                right,
            } => write!(f, "({left} {operator} {right})"),
            Self::If {
                condition,
                consequence,
                alternative,
            } => match alternative.as_ref() {
                Some(alternative) => write!(f, "if {condition} {consequence} else {alternative}"),
                None => write!(f, "if {condition} {consequence}"),
            },
            Self::Function { parameters, body } => {
                let params: String = parameters
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "fn({params}) {body} ")
            }
            Self::Block { statements } => {
                let s: String = statements.iter().map(|s| s.to_string()).collect();
                write!(f, "{s}")
            }
            Self::Call {
                function,
                arguments,
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
