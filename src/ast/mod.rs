mod block;
mod expression;
mod identifier;
mod statement;

pub use block::Block;
pub use expression::Expression;
pub use identifier::Identifier;
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

use std::fmt::Display;

pub trait Node: Display {
    fn token_literal(&self) -> &str;
}

pub trait StatementNode: Node {
    fn statement_node(&self) {}
}

pub trait ExpressionNode: Node {
    fn expression_node(&self) {}
}

use std::fmt;

pub struct Program<'a> {
    pub statements: Vec<Statement<'a>>,
}

impl<'a> Program<'a> {
    pub fn new(statements: Vec<Statement<'a>>) -> Self {
        Self { statements }
    }
}

impl<'a> Node for Program<'a> {
    fn token_literal(&self) -> &str {
        self.statements
            .first()
            .map(|stmt| stmt.token_literal())
            .unwrap_or_default()
    }
}

impl<'a> fmt::Display for Program<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{stmt}")?;
        }
        Ok(())
    }
}
