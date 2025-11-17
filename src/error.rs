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
    NotAFunction {
        function: ObjectType,
    },
    IdentifierNotFound {
        name: String,
    },
    InvalidArgumentCount {
        expected: usize,
        got: usize,
    },
    InvalidArgumentType {
        expected: ObjectType,
        got: ObjectType,
    },
}

impl Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
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
                write!(f, "unsupported prefix operator: {operator}{right:?}")
            }
            EvalError::DivisionByZero => write!(f, "division by zero"),
            EvalError::NotAFunction { function } => write!(f, "not a function: {function:?}"),
            EvalError::IdentifierNotFound { name } => write!(f, "identifier not found: {name}"),
            EvalError::InvalidArgumentCount { expected, got } => {
                let arg = if *expected == 1 {
                    "argument"
                } else {
                    "arguments"
                };

                write!(f, "expected {expected} {arg}, got {got} instead")
            }
            EvalError::InvalidArgumentType { expected, got } => {
                write!(f, "expected {expected:?}, got {got:?} instead")
            }
        }
    }
}
