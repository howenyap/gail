use crate::token::Token;

pub struct Program<'a> {
    pub statements: Vec<Statement<'a>>,
}

impl<'a> Program<'a> {
    pub fn token_literal(&self) -> &str {
        self.statements
            .first()
            .map(|stmt| stmt.token_literal())
            .unwrap_or_default()
    }
}

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Let(LetStatement<'a>),
}

impl<'a> Statement<'a> {
    pub fn token_literal(&self) -> &str {
        match self {
            Statement::Let(stmt) => stmt.token_literal(),
        }
    }

    pub fn statement_node(&self) {}
}

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    Identifier(Identifier<'a>),
    Empty,
}

impl<'a> Expression<'a> {
    pub fn new(token: Token<'a>) -> Self {
        Self::Identifier(Identifier::new(token))
    }

    pub fn token_literal(&self) -> &str {
        match self {
            Expression::Identifier(expr) => expr.token_literal(),
            Expression::Empty => "empty expression",
        }
    }

    pub fn expression_node(&self) {}
}

#[derive(Debug, PartialEq)]
pub struct LetStatement<'a> {
    pub token: Token<'a>,
    pub name: Identifier<'a>,
    pub value: Expression<'a>,
}

impl<'a> LetStatement<'a> {
    pub fn new(token: Token<'a>, name: Identifier<'a>, value: Expression<'a>) -> Self {
        Self { token, name, value }
    }

    pub fn token_literal(&self) -> &str {
        self.token.literal()
    }

    // pub fn statement_node(&self) {}
}

#[derive(Debug, PartialEq)]
pub struct Identifier<'a> {
    pub token: Token<'a>,
    pub value: &'a str,
}

impl<'a> Identifier<'a> {
    pub fn new(token: Token<'a>) -> Self {
        Self {
            token,
            value: token.literal,
        }
    }

    pub fn token_literal(&self) -> &str {
        self.token.literal()
    }

    // pub fn expression_node(&self) {}
}
