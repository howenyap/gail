use crate::object::ObjectType;
use crate::token::{Token, TokenType};
use std::fmt::{self, Display};

#[derive(Debug, PartialEq)]
pub enum ParseError<'a> {
    UnexpectedToken {
        found: Token<'a>,
        expected: TokenType,
    },
    InvalidInteger,
    UnknownPrefixOperator {
        token: Token<'a>,
    },
    UnknownInfixOperator {
        token: Token<'a>,
    },
}

impl<'a> Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken { found, expected } => {
                write!(
                    f,
                    "expected next token to be {expected:?}, got {found} instead"
                )
            }
            ParseError::InvalidInteger => write!(f, "invalid integer"),
            ParseError::UnknownPrefixOperator { token } => {
                write!(f, "unknown prefix operator: {token}")
            }
            ParseError::UnknownInfixOperator { token } => {
                write!(f, "unknown infix operator: {token}")
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum EvalError {
    UnexpectedType {
        found: ObjectType,
        expected: ObjectType,
    },
    UnsupportedInfixOperator {
        left: ObjectType,
        right: ObjectType,
        operator: String,
    },
    UnsupportedPrefixOperator {
        right: ObjectType,
        operator: String,
    },
    DivisionByZero,
}

impl Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::UnexpectedType { found, expected } => {
                write!(f, "expected type {expected:?}, got {found:?} instead")
            }
            EvalError::UnsupportedInfixOperator {
                left,
                right,
                operator,
            } => {
                write!(
                    f,
                    "unsupported infix operator: {left:?} {operator} {right:?}"
                )
            }
            EvalError::UnsupportedPrefixOperator { right, operator } => {
                write!(f, "unsupported prefix operator: {operator} {right:?}")
            }
            EvalError::DivisionByZero => write!(f, "division by zero"),
        }
    }
}
