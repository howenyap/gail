use crate::object::{Object, ObjectType};
use crate::token::TokenType;
use crate::utils::{join_with_separator, to_plural};
use thiserror::Error;

#[derive(Debug, PartialEq, Clone, Error)]
pub enum ParseError {
    #[error("expected next token to be {expected:?}, got {found} instead")]
    UnexpectedToken { found: String, expected: TokenType },
    #[error("invalid integer")]
    InvalidInteger,
    #[error("unknown prefix operator: {token}")]
    UnknownPrefixOperator { token: String },
    #[error("unknown infix operator: {token}")]
    UnknownInfixOperator { token: String },
}

#[derive(Debug, PartialEq, Error)]
pub enum EvalError {
    #[error("unsupported infix operator: {left:?} {operator} {right:?}")]
    UnsupportedInfixOperator {
        left: ObjectType,
        right: ObjectType,
        operator: String,
    },
    #[error("unsupported prefix operator: {operator}{right:?}")]
    UnsupportedPrefixOperator { right: ObjectType, operator: String },
    #[error("division by zero")]
    DivisionByZero,
    #[error("not a function: {function:?}")]
    NotAFunction { function: ObjectType },
    #[error("identifier not found: {name}")]
    IdentifierNotFound { name: String },
    #[error(
        "expected {} {}, got {} instead",
        expected,
        to_plural("argument", *expected),
        got
    )]
    InvalidArgumentCount { expected: usize, got: usize },
    #[error("expected {expected:?}, got {got:?} instead")]
    ExpectedType {
        expected: ObjectType,
        got: ObjectType,
    },
    #[error(
        "expected {}, got {got:?} instead",
        join_with_separator(expected, " or ")
    )]
    ExpectedMultipleTypes {
        expected: &'static [ObjectType],
        got: ObjectType,
    },
    #[error(
        "not indexable: only {} can be indexed, got {object:?} instead",
        join_with_separator(Object::indexable_types(), " or ")
    )]
    NotIndexable { object: ObjectType },
    #[error("index out of bounds: object has length {length}, but index is {index}")]
    IndexOutOfBounds { index: usize, length: usize },
    #[error("index out of range: index must be between 0 and {length}, got {index} instead")]
    IndexOutOfRange { index: i64, length: usize },
    #[error(
        "count out of range: count must be between 0 and {}, got {}",
        usize::MAX,
        value
    )]
    UsizeOutOfRange { value: i64 },
    #[error("empty array")]
    EmptyArray,
    #[error("invalid range: range must be between 0..{limit}, got {}..{} instead", range.0, range.1)]
    InvalidRange { range: (i64, i64), limit: i64 },
}

impl EvalError {
    pub fn expected_type(expected: ObjectType, got: ObjectType) -> Self {
        Self::ExpectedType { expected, got }
    }

    pub fn invalid_argument_count(expected: usize, got: usize) -> Self {
        Self::InvalidArgumentCount { expected, got }
    }
}

#[derive(Error, Debug)]
pub enum InterpreterError {
    #[error("parsing error: {0:#?}")]
    Parse(#[from] ParseErrors),
    #[error("evaluation error: {0}")]
    Eval(#[from] EvalError),
}

#[derive(Debug, Error)]
#[error("{0:?}")]
pub struct ParseErrors(Vec<ParseError>);

impl ParseErrors {
    pub fn new(errors: Vec<ParseError>) -> Self {
        Self(errors)
    }
}
