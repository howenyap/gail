use std::fmt::{self, Debug, Display};

use crate::error::EvalError;

#[derive(Debug)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}

#[derive(Debug, PartialEq)]
pub enum ObjectType {
    Integer,
    Boolean,
    Null,
}

pub trait ObjectTrait {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

impl Object {
    pub fn is_null(&self) -> bool {
        matches!(self, Object::Null)
    }

    pub fn is_truthy(&self) -> bool {
        if matches!(self, Object::Boolean(false) | Object::Null) {
            false
        } else {
            true
        }
    }

    pub fn r#true() -> Self {
        Object::Boolean(true)
    }

    pub fn r#false() -> Self {
        Object::Boolean(false)
    }

    pub fn add(self, other: Self) -> Result<Self, EvalError> {
        let evaluated = match (self, other) {
            (Object::Integer(left), Object::Integer(right)) => Object::Integer(left + right),
            _ => {
                return Err(EvalError::UnsupportedInfixOperator {
                    left: ObjectType::Integer,
                    right: ObjectType::Integer,
                    operator: "+".to_string(),
                });
            }
        };

        Ok(evaluated)
    }

    pub fn subtract(self, other: Self) -> Result<Self, EvalError> {
        let evaluated = match (self, other) {
            (Object::Integer(left), Object::Integer(right)) => Object::Integer(left - right),
            _ => {
                return Err(EvalError::UnsupportedInfixOperator {
                    left: ObjectType::Integer,
                    right: ObjectType::Integer,
                    operator: "-".to_string(),
                });
            }
        };

        Ok(evaluated)
    }

    pub fn multiply(self, other: Self) -> Result<Self, EvalError> {
        let evaluated = match (self, other) {
            (Object::Integer(left), Object::Integer(right)) => Object::Integer(left * right),
            _ => {
                return Err(EvalError::UnsupportedInfixOperator {
                    left: ObjectType::Integer,
                    right: ObjectType::Integer,
                    operator: "*".to_string(),
                });
            }
        };

        Ok(evaluated)
    }

    pub fn divide(self, other: Self) -> Result<Self, EvalError> {
        let evaluated = match (self, other) {
            (Object::Integer(left), Object::Integer(right)) => Object::Integer(left / right),
            _ => {
                return Err(EvalError::UnsupportedInfixOperator {
                    left: ObjectType::Integer,
                    right: ObjectType::Integer,
                    operator: "/".to_string(),
                });
            }
        };

        Ok(evaluated)
    }

    pub fn less_than(self, other: Self) -> Result<Self, EvalError> {
        let evaluated = match (self, other) {
            (Object::Integer(left), Object::Integer(right)) => Object::Boolean(left < right),
            _ => {
                return Err(EvalError::UnsupportedInfixOperator {
                    left: ObjectType::Integer,
                    right: ObjectType::Integer,
                    operator: "<".to_string(),
                });
            }
        };

        Ok(evaluated)
    }

    pub fn greater_than(self, other: Self) -> Result<Self, EvalError> {
        let evaluated = match (&self, &other) {
            (Object::Integer(left), Object::Integer(right)) => Object::Boolean(left > right),
            _ => {
                return Err(EvalError::UnsupportedInfixOperator {
                    left: self.object_type(),
                    right: other.object_type(),
                    operator: ">".to_string(),
                });
            }
        };

        Ok(evaluated)
    }

    pub fn equal(self, other: Self) -> Result<Self, EvalError> {
        let evaluated = match (&self, &other) {
            (Object::Integer(left), Object::Integer(right)) => Object::Boolean(left == right),
            (Object::Boolean(left), Object::Boolean(right)) => Object::Boolean(left == right),
            _ => {
                return Err(EvalError::UnsupportedInfixOperator {
                    left: self.object_type(),
                    right: other.object_type(),
                    operator: "==".to_string(),
                });
            }
        };

        Ok(evaluated)
    }

    pub fn not_equal(self, other: Self) -> Result<Self, EvalError> {
        let evaluated = match (&self, &other) {
            (Object::Integer(left), Object::Integer(right)) => Object::Boolean(left != right),
            (Object::Boolean(left), Object::Boolean(right)) => Object::Boolean(left != right),
            _ => {
                return Err(EvalError::UnsupportedInfixOperator {
                    left: self.object_type(),
                    right: other.object_type(),
                    operator: "!=".to_string(),
                });
            }
        };

        Ok(evaluated)
    }

    pub fn bang(self) -> Result<Self, EvalError> {
        let evaluated = match self {
            Object::Boolean(value) => Object::Boolean(!value),
            Object::Null => Object::r#true(),
            _ => Object::r#false(),
        };

        Ok(evaluated)
    }

    pub fn negate(self) -> Result<Self, EvalError> {
        let evaluated = match self {
            Object::Integer(value) => Object::Integer(-value),
            _ => {
                return Err(EvalError::UnexpectedType {
                    found: self.object_type(),
                    expected: ObjectType::Integer,
                });
            }
        };

        Ok(evaluated)
    }
}

impl ObjectTrait for Object {
    fn object_type(&self) -> ObjectType {
        match self {
            Object::Integer(_) => ObjectType::Integer,
            Object::Boolean(_) => ObjectType::Boolean,
            Object::Null => ObjectType::Null,
        }
    }

    fn inspect(&self) -> String {
        match self {
            Object::Integer(value) => value.to_string(),
            Object::Boolean(value) => value.to_string(),
            Object::Null => "null".to_string(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inspect())
    }
}
