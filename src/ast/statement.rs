use crate::ast::Expression;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let {
        // invariant: expression must be Identifier
        name: Expression,
        value: Expression,
    },
    Return {
        value: Expression,
    },
    Expression {
        expression: Expression,
    },
}

impl Statement {
    pub fn r#let(name: Expression, value: Expression) -> Self {
        Self::Let { name, value }
    }

    pub fn r#return(value: Expression) -> Self {
        Self::Return { value }
    }

    pub fn expression(expression: Expression) -> Self {
        Self::Expression { expression }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let { name, value } => write!(f, "let {name} = {value};"),
            Statement::Return { value } => write!(f, "return {value};"),
            Statement::Expression { expression } => write!(f, "{expression}"),
        }
    }
}
