mod expression;
mod statement;

pub use expression::Expression;
pub use statement::Statement;

use crate::error::ParseErrors;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::fmt;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
    Hash,
}

impl Default for Precedence {
    fn default() -> Self {
        Self::Lowest
    }
}

#[derive(Debug, PartialEq)]
pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self { statements }
    }

    pub fn from_input(input: &str) -> Result<Self, ParseErrors> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if let Some(errors) = parser.errors() {
            Err(ParseErrors::new(errors))
        } else {
            Ok(program)
        }
    }

    pub fn statements(&self) -> impl Iterator<Item = &Statement> {
        self.statements.iter()
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in self.statements.iter() {
            write!(f, "{stmt}")?;
        }

        Ok(())
    }
}
