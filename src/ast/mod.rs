mod expression;
mod statement;

pub use expression::Expression;
pub use statement::Statement;

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
pub struct Program<'a> {
    statements: Vec<Statement<'a>>,
}

impl<'a> Program<'a> {
    pub fn new(statements: Vec<Statement<'a>>) -> Self {
        Self { statements }
    }

    pub fn statements(&self) -> &[Statement<'a>] {
        &self.statements
    }

    pub fn token_literal(&self) -> &str {
        self.statements
            .first()
            .map(|stmt| stmt.token_literal())
            .unwrap_or_default()
    }
}

impl<'a> From<Program<'a>> for Node<'a> {
    fn from(program: Program<'a>) -> Self {
        Node::Program(program)
    }
}

#[derive(Debug, PartialEq)]
pub enum Node<'a> {
    Program(Program<'a>),
    Statement(Statement<'a>),
    Expression(Expression<'a>),
}

impl<'a> Node<'a> {
    pub fn token_literal(&self) -> &str {
        match self {
            Node::Program(p) => p.token_literal(),
            Node::Statement(s) => s.token_literal(),
            Node::Expression(e) => e.token_literal(),
        }
    }
}

impl<'a> fmt::Display for Program<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in self.statements.iter() {
            write!(f, "{stmt}")?;
        }

        Ok(())
    }
}

impl<'a> fmt::Display for Node<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Program(p) => write!(f, "{p}"),
            Node::Statement(s) => write!(f, "{s}"),
            Node::Expression(e) => write!(f, "{e}"),
        }
    }
}
