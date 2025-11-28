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
    Array {
        elements: Vec<Expression>,
    },
    Index {
        // invariant: expression must be Array
        left: Box<Expression>,
        // invariant: expression must be Integer
        index: Box<Expression>,
    },
    HashMap {
        pairs: Vec<(Expression, Expression)>,
    },
}

impl Expression {
    pub fn ident(value: String) -> Self {
        Self::Ident { value }
    }

    pub fn int(value: i64) -> Self {
        Self::Int { value }
    }

    pub fn bool(value: bool) -> Self {
        Self::Bool { value }
    }

    pub fn string(value: String) -> Self {
        Self::String { value }
    }

    pub fn prefix(operator: String, right: Expression) -> Self {
        Self::Prefix {
            operator,
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

    pub fn infix(operator: String, left: Expression, right: Expression) -> Self {
        Self::Infix {
            left: Box::new(left),
            operator,
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

    pub fn array(elements: Vec<Expression>) -> Self {
        Self::Array { elements }
    }

    pub fn index(left: Expression, index: Expression) -> Self {
        Self::Index {
            left: Box::new(left),
            index: Box::new(index),
        }
    }

    pub fn hashmap(pairs: Vec<(Expression, Expression)>) -> Self {
        Self::HashMap { pairs }
    }

    // note:
    // only primitive types like integers, booleans, and strings are hashable
    // arrays and hashmaps are not hashable
    // anything else will evalute to a primitive type
    pub fn hashable(&self) -> bool {
        !matches!(
            self,
            Expression::Array { .. } | Expression::HashMap { .. } | Expression::Function { .. }
        )
    }
}

fn comma_separated<T: Display>(items: &[T]) -> String {
    items
        .iter()
        .map(|item| item.to_string())
        .collect::<Vec<String>>()
        .join(", ")
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
                let params = comma_separated(parameters);
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
                let args = comma_separated(arguments);
                write!(f, "{function}({args})")
            }
            Self::Array { elements } => {
                let elements = comma_separated(elements);
                write!(f, "[{elements}]")
            }
            Self::Index { left, index } => {
                let left = left.to_string();
                let index = index.to_string();

                write!(f, "({left}[{index}])")
            }
            Self::HashMap { pairs } => {
                let pairs = pairs
                    .iter()
                    .map(|(key, value)| format!("{key}: {value}"))
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "{{{pairs}}}")
            }
        }
    }
}
