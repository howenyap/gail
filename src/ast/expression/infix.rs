use crate::ast::Expression;
use crate::ast::ExpressionNode;
use crate::ast::Node;
use crate::token::Token;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq)]
pub struct InfixExpression<'a> {
    pub token: Token<'a>,
    pub left: Box<Expression<'a>>,
    pub operator: &'a str,
    pub right: Box<Expression<'a>>,
}

impl<'a> InfixExpression<'a> {
    pub fn new(
        token: Token<'a>,
        left: Expression<'a>,
        operator: &'a str,
        right: Expression<'a>,
    ) -> Self {
        let left = Box::new(left);
        let right = Box::new(right);
        Self {
            token,
            left,
            operator,
            right,
        }
    }
}

impl<'a> Node for InfixExpression<'a> {
    fn token_literal(&self) -> &str {
        self.token.literal()
    }
}

impl<'a> Display for InfixExpression<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

impl<'a> ExpressionNode for InfixExpression<'a> {
    fn expression_node(&self) {}
}
