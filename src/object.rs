use std::fmt::{self, Debug, Display};

use crate::ast::Expression;
use crate::environment::Env;
use crate::error::EvalError;
use crate::utils::join_with_separator;

#[derive(Debug, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Object>),
    Null,
    Unit,
    ReturnValue(Box<Object>),
    Function {
        parameters: Vec<String>,
        // invariant: expression must be Block
        body: Box<Expression>,
        env: Env,
    },
    BuiltinFunction(FunctionType),
    HashMap(Vec<(Object, Object)>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionType {
    Len,
    Push,
    First,
    Last,
    Rest,
    Slice,
    Print,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ObjectType {
    Integer,
    Boolean,
    String,
    Array,
    Null,
    Unit,
    ReturnValue,
    Function,
    BuiltinFunction,
    HashMap,
}

pub trait ObjectTrait {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        !matches!(
            self,
            Object::Boolean(false) | Object::Null | Object::Integer(0)
        )
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

    #[allow(clippy::should_implement_trait)]
    pub fn add(self, other: Self) -> Result<Self, EvalError> {
        let evaluated = match (self, other) {
            (Object::Integer(left), Object::Integer(right)) => Object::Integer(left + right),
            (Object::String(left), Object::String(right)) => Object::String(left + &right),
            (Object::Array(left), Object::Array(right)) => {
                Object::Array(left.into_iter().chain(right).collect())
            }
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
            (Object::Integer(left), Object::String(right)) => {
                let times: usize = left
                    .try_into()
                    .map_err(|_| EvalError::UsizeOutOfRange { value: left })?;

                Object::String(right.repeat(times))
            }
            (Object::Integer(left), Object::Array(right)) => {
                let length = right.len();
                let times: usize = left
                    .try_into()
                    .map_err(|_| EvalError::UsizeOutOfRange { value: left })?;

                Object::Array(right.into_iter().cycle().take(times * length).collect())
            }
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

    pub fn greater_than_or_equal(self, other: Self) -> Result<Self, EvalError> {
        let evaluated = match (self, other) {
            (Object::Integer(left), Object::Integer(right)) => Object::Boolean(left >= right),
            (left, right) => {
                return Err(EvalError::UnsupportedInfixOperator {
                    left: left.object_type(),
                    right: right.object_type(),
                    operator: ">=".to_string(),
                });
            }
        };

        Ok(evaluated)
    }

    pub fn less_than_or_equal(self, other: Self) -> Result<Self, EvalError> {
        let evaluated = match (self, other) {
            (Object::Integer(left), Object::Integer(right)) => Object::Boolean(left <= right),
            (left, right) => {
                return Err(EvalError::UnsupportedInfixOperator {
                    left: left.object_type(),
                    right: right.object_type(),
                    operator: "<=".to_string(),
                });
            }
        };

        Ok(evaluated)
    }

    pub fn equal(&self, other: &Self) -> Result<Self, EvalError> {
        let evaluated = match (self, other) {
            (Object::Integer(left), Object::Integer(right)) => Object::Boolean(left == right),
            (Object::Boolean(left), Object::Boolean(right)) => Object::Boolean(left == right),
            (Object::String(left), Object::String(right)) => Object::Boolean(left == right),
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
            (Object::String(left), Object::String(right)) => Object::Boolean(left != right),
            (Object::Null, Object::Null) => Object::Boolean(false),

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
                return Err(EvalError::UnsupportedPrefixOperator {
                    right: self.object_type(),
                    operator: "-".to_string(),
                });
            }
        };

        Ok(evaluated)
    }

    pub fn index(&self, index: Object) -> Result<Object, EvalError> {
        match self {
            Object::Array(value) => {
                let index = index.expect_integer()?;
                let index = self.validate_index(index, value.len())?;
                // safety: validate_index checks bounds
                let element = value[index].clone();

                Ok(element)
            }
            Object::String(value) => {
                let index = index.expect_integer()?;
                let index = self.validate_index(index, value.len())?;
                // safety: validate_index checks bounds
                // note: only correct for ascii strings
                let char = value.chars().nth(index).unwrap();

                Ok(Object::String(char.to_string()))
            }
            Object::HashMap(value) => {
                if !index.is_hashable() {
                    return Err(EvalError::UnhashableObject {
                        object: index.object_type(),
                    });
                }

                let element = value
                    .iter()
                    .find(|(key, _)| {
                        key.equal(&index)
                            .is_ok_and(|o| matches!(o, Object::Boolean(true)))
                    })
                    .map(|(_, value)| value.clone())
                    .unwrap_or(Object::Null);

                Ok(element)
            }
            _ => Err(EvalError::NotIndexable {
                object: self.object_type(),
            }),
        }
    }

    // checks if i64 can be converted to usize, and bounds checks
    fn validate_index(&self, index: i64, length: usize) -> Result<usize, EvalError> {
        let index = if index >= 0 {
            index
        } else {
            length as i64 + index
        };

        let Ok(index) = index.try_into() else {
            return Err(EvalError::IndexOutOfRange { index, length });
        };

        if index >= length {
            return Err(EvalError::IndexOutOfBounds { index, length });
        }

        Ok(index)
    }

    pub fn indexable_types() -> &'static [ObjectType] {
        &[ObjectType::String, ObjectType::Array]
    }

    pub fn length(&self) -> Option<usize> {
        match self {
            Object::String(value) => Some(value.len()),
            Object::Array(value) => Some(value.len()),
            _ => None,
        }
    }

    pub fn is_hashable(&self) -> bool {
        matches!(
            self,
            Object::Integer(_) | Object::Boolean(_) | Object::String(_)
        )
    }

    pub fn expect_integer(self) -> Result<i64, EvalError> {
        match self {
            Object::Integer(value) => Ok(value),
            obj => Err(EvalError::expected_type(
                ObjectType::Integer,
                obj.object_type(),
            )),
        }
    }

    pub fn expect_array(self) -> Result<Vec<Object>, EvalError> {
        match self {
            Object::Array(value) => Ok(value),
            obj => Err(EvalError::expected_type(
                ObjectType::Array,
                obj.object_type(),
            )),
        }
    }

    pub fn expect_string(self) -> Result<String, EvalError> {
        match self {
            Object::String(value) => Ok(value),
            obj => Err(EvalError::expected_type(
                ObjectType::String,
                obj.object_type(),
            )),
        }
    }

    pub fn expect_boolean(self) -> Result<bool, EvalError> {
        match self {
            Object::Boolean(value) => Ok(value),
            obj => Err(EvalError::expected_type(
                ObjectType::Boolean,
                obj.object_type(),
            )),
        }
    }
}

impl ObjectTrait for Object {
    fn object_type(&self) -> ObjectType {
        match self {
            Object::Integer(_) => ObjectType::Integer,
            Object::Boolean(_) => ObjectType::Boolean,
            Object::String(_) => ObjectType::String,
            Object::Null => ObjectType::Null,
            Object::Unit => ObjectType::Unit,
            Object::ReturnValue(_) => ObjectType::ReturnValue,
            Object::Function { .. } => ObjectType::Function,
            Object::BuiltinFunction { .. } => ObjectType::BuiltinFunction,
            Object::Array(_) => ObjectType::Array,
            Object::HashMap(_) => ObjectType::HashMap,
        }
    }

    fn inspect(&self) -> String {
        match self {
            Object::Integer(value) => value.to_string(),
            Object::Boolean(value) => value.to_string(),
            Object::String(value) => value.to_string(),
            Object::Array(value) => {
                let elements = join_with_separator(value, ", ");

                format!("[{elements}]")
            }
            Object::Null => "null".to_string(),
            Object::Unit => "()".to_string(),
            Object::ReturnValue(value) => value.inspect(),
            Object::Function {
                parameters, body, ..
            } => {
                let params = join_with_separator(parameters, ", ");

                format!("fn({params}) {{\n{body}\n}}")
            }
            Object::BuiltinFunction(function_type) => function_type.to_string(),
            Object::HashMap(pairs) => {
                let pairs = pairs
                    .iter()
                    .map(|(key, value)| format!("{key}: {value}"))
                    .collect::<Vec<String>>()
                    .join(", ");

                format!("{{{pairs}}}")
            }
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inspect())
    }
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = format!("{self:?}").to_lowercase();
        write!(f, "builtin function: {name}")
    }
}
