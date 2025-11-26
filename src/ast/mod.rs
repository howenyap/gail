mod expression;
mod statement;

pub use expression::Expression;
pub use statement::Statement;

use crate::lexer::Lexer;
use crate::parser::Parser;

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
}

impl Default for Precedence {
    fn default() -> Self {
        Self::Lowest
    }
}

use std::fmt;

#[derive(Debug, PartialEq)]
pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self { statements }
    }

    #[allow(dead_code)]
    pub fn from_str(input: &str) -> Result<Self, &'static str> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if parser.has_errors() {
            parser.print_errors();
            Err("Failed to parse program")
        } else {
            Ok(program)
        }
    }

    pub fn statements(&self) -> &[Statement] {
        &self.statements
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
