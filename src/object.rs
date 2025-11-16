use std::fmt::{self, Debug, Display};

use crate::ast::Expression;
use crate::environment::Environment;
use crate::error::EvalError;

#[derive(Debug, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Function {
        parameters: Vec<String>,
        body: Box<Expression>,
        env: Environment,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum ObjectType {
    Integer,
    Boolean,
    Null,
    ReturnValue,
    Function,
}

pub trait ObjectTrait {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

impl Object {
    pub fn is_null(&self) -> bool {
        matches!(self, Object::Null)
    }

    pub fn is_return_value(&self) -> bool {
        matches!(self, Object::ReturnValue(_))
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Object::Boolean(false) | Object::Null)
    }

    pub fn r#true() -> Self {
        Object::Boolean(true)
    }

    pub fn r#false() -> Self {
        Object::Boolean(false)
    }

    pub fn return_value(value: Object) -> Self {
        Object::ReturnValue(Box::new(value))
    }

    pub fn add(self, other: Self) -> Result<Self, EvalError> {
        let evaluated = match (self, other) {
            (Object::Integer(left), Object::Integer(right)) => Object::Integer(left + right),
            (left, right) => {
                return Err(EvalError::UnsupportedInfixOperator {
                    left: left.object_type(),
                    right: right.object_type(),
                    operator: "+".to_string(),
                });
            }
        };

        Ok(evaluated)
    }

    pub fn subtract(self, other: Self) -> Result<Self, EvalError> {
        let evaluated = match (self, other) {
            (Object::Integer(left), Object::Integer(right)) => Object::Integer(left - right),
            (left, right) => {
                return Err(EvalError::UnsupportedInfixOperator {
                    left: left.object_type(),
                    right: right.object_type(),
                    operator: "-".to_string(),
                });
            }
        };

        Ok(evaluated)
    }

    pub fn multiply(self, other: Self) -> Result<Self, EvalError> {
        let evaluated = match (self, other) {
            (Object::Integer(left), Object::Integer(right)) => Object::Integer(left * right),
            (left, right) => {
                return Err(EvalError::UnsupportedInfixOperator {
                    left: left.object_type(),
                    right: right.object_type(),
                    operator: "*".to_string(),
                });
            }
        };

        Ok(evaluated)
    }

    pub fn divide(self, other: Self) -> Result<Self, EvalError> {
        let evaluated = match (self, other) {
            (Object::Integer(left), Object::Integer(right)) => {
                if right == 0 {
                    return Err(EvalError::DivisionByZero);
                }

                Object::Integer(left / right)
            }
            (left, right) => {
                return Err(EvalError::UnsupportedInfixOperator {
                    left: left.object_type(),
                    right: right.object_type(),
                    operator: "/".to_string(),
                });
            }
        };

        Ok(evaluated)
    }

    pub fn less_than(self, other: Self) -> Result<Self, EvalError> {
        let evaluated = match (self, other) {
            (Object::Integer(left), Object::Integer(right)) => Object::Boolean(left < right),
            (left, right) => {
                return Err(EvalError::UnsupportedInfixOperator {
                    left: left.object_type(),
                    right: right.object_type(),
                    operator: "<".to_string(),
                });
            }
        };

        Ok(evaluated)
    }

    pub fn greater_than(self, other: Self) -> Result<Self, EvalError> {
        let evaluated = match (self, other) {
            (Object::Integer(left), Object::Integer(right)) => Object::Boolean(left > right),
            (left, right) => {
                return Err(EvalError::UnsupportedInfixOperator {
                    left: left.object_type(),
                    right: right.object_type(),
                    operator: ">".to_string(),
                });
            }
        };

        Ok(evaluated)
    }

    pub fn equal(self, other: Self) -> Result<Self, EvalError> {
        let evaluated = match (self, other) {
            (Object::Integer(left), Object::Integer(right)) => Object::Boolean(left == right),
            (Object::Boolean(left), Object::Boolean(right)) => Object::Boolean(left == right),
            (left, right) => {
                return Err(EvalError::UnsupportedInfixOperator {
                    left: left.object_type(),
                    right: right.object_type(),
                    operator: "==".to_string(),
                });
            }
        };

        Ok(evaluated)
    }

    pub fn not_equal(self, other: Self) -> Result<Self, EvalError> {
        let evaluated = match (self, other) {
            (Object::Integer(left), Object::Integer(right)) => Object::Boolean(left != right),
            (Object::Boolean(left), Object::Boolean(right)) => Object::Boolean(left != right),
            (left, right) => {
                return Err(EvalError::UnsupportedInfixOperator {
                    left: left.object_type(),
                    right: right.object_type(),
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
            Object::ReturnValue(_) => ObjectType::ReturnValue,
            Object::Function { .. } => ObjectType::Function,
        }
    }

    fn inspect(&self) -> String {
        match self {
            Object::Integer(value) => value.to_string(),
            Object::Boolean(value) => value.to_string(),
            Object::Null => "null".to_string(),
            Object::ReturnValue(value) => value.inspect(),
            Object::Function {
                parameters, body, ..
            } => {
                let params: String = parameters
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");

                format!("fn({params}) {{\n{body}\n}}")
            }
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inspect())
    }
}
