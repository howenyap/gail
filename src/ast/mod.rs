mod expression;
mod statement;

pub use expression::Expression;
pub use statement::Statement;

use crate::error::ProgramError;
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

    pub fn from_str(input: &str) -> Result<Self, ProgramError> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if parser.has_errors() {
            parser.print_errors();
            Err(ProgramError::Parse)
        } else {
            Ok(program)
        }
    }

    pub fn statements(&self) -> &[Statement] {
        &self.statements
    }
}

impl From<Program> for Node {
    fn from(program: Program) -> Self {
        Node::Program(program)
    }
}

#[derive(Debug, PartialEq)]
pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in self.statements.iter() {
            write!(f, "{stmt}")?;
        }

        Ok(())
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Program(p) => write!(f, "{p}"),
            Node::Statement(s) => write!(f, "{s}"),
            Node::Expression(e) => write!(f, "{e}"),
        }
    }
}
