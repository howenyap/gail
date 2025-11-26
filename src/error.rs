use crate::object::{Object, ObjectType};
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
    ExpectedType {
        expected: ObjectType,
        got: ObjectType,
    },
    ExpectedMultipleTypes {
        expected: &'static [ObjectType],
        got: ObjectType,
    },
    NotIndexable {
        object: ObjectType,
    },
    IndexOutOfBounds {
        index: usize,
        length: usize,
    },
    IndexOutOfRange {
        index: i64,
    },
    UsizeOutOfRange {
        value: i64,
    },
    EmptyArray,
}

impl EvalError {
    pub fn expected_type(expected: ObjectType, got: ObjectType) -> Self {
        Self::ExpectedType { expected, got }
    }

    pub fn invalid_argument_count(expected: usize, got: usize) -> Self {
        Self::InvalidArgumentCount { expected, got }
    }
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
            EvalError::ExpectedType { expected, got } => {
                write!(f, "expected {expected:?}, got {got:?} instead")
            }
            EvalError::ExpectedMultipleTypes { expected, got } => {
                let expected = expected
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(" or ");

                write!(f, "expected {expected}, got {got:?} instead")
            }
            EvalError::NotIndexable { object } => {
                let indexable_types = Object::indexable_types()
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(" or ");

                write!(
                    f,
                    "not indexable: only {indexable_types} can be indexed, got {object:?} instead",
                )
            }
            EvalError::IndexOutOfBounds { index, length } => {
                write!(
                    f,
                    "index out of bounds: object has length {length}, but index is {index}"
                )
            }
            EvalError::IndexOutOfRange { index } => {
                write!(
                    f,
                    "index out of range: index must be between 0 and {}, got {}",
                    usize::MAX,
                    index
                )
            }
            EvalError::UsizeOutOfRange { value } => {
                write!(
                    f,
                    "count out of range: count must be between 0 and {}, got {}",
                    usize::MAX,
                    value
                )
            }
            EvalError::EmptyArray => write!(f, "empty array"),
        }
    }
}
