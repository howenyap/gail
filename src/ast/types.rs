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
