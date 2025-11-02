use crate::ast::{Expression, Identifier, LetStatement, Program, ReturnStatement, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use crate::token::TokenType;
use std::fmt::{self, Display};

#[derive(Debug, PartialEq)]
pub enum ParseError<'a> {
    UnexpectedToken {
        found: Token<'a>,
        expected: TokenType,
    },
    UnknownStatement {
        token: Token<'a>,
    },
    UnexpectedEOF {
        expected: TokenType,
    },
}

impl<'a> Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            ParseError::UnexpectedToken { found, expected } => {
                format!("expected next token to be {expected:?}, got {found} instead")
            }
            ParseError::UnknownStatement { token } => {
                format!("unknown statement: {token}")
            }
            ParseError::UnexpectedEOF { expected } => {
                format!("expected token to be {expected:?} but reached EOF instead")
            }
        };

        write!(f, "{s}")
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token<'a>,
    peek_token: Token<'a>,
    errors: Vec<ParseError<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        let first = lexer.next_token();
        let second = lexer.next_token();
        let errors = vec![];

        Self {
            lexer,
            current_token: first,
            peek_token: second,
            errors,
        }
    }

    pub fn next_token(&mut self) {
        let next_token = self.lexer.next_token();
        self.current_token = self.peek_token;
        self.peek_token = next_token;
    }

    pub fn parse_program(&mut self) -> Result<Program<'a>, ParseError<'a>> {
        let mut statements = vec![];

        while !self.current_token.is_eof() {
            match self.parse_statement() {
                Ok(statement) => statements.push(statement),
                Err(error) => self.errors.push(error),
            }
            self.next_token();
        }

        Ok(Program { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement<'a>, ParseError<'a>> {
        match self.current_token.token_type() {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => Err(ParseError::UnknownStatement {
                token: self.current_token,
            }),
        }
    }

    fn parse_expression(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        Ok(Expression::Empty)
    }

    fn parse_let_statement(&mut self) -> Result<Statement<'a>, ParseError<'a>> {
        let let_token = self.current_token;

        let ident_token = self.expect_peek(TokenType::Ident)?;
        let name = Identifier::new(ident_token);

        self.expect_peek(TokenType::Assign)?;

        let value = self.parse_expression()?;
        let let_statement = LetStatement::new(let_token, name, value);

        self.advance_until_current_is(TokenType::Semicolon)?;

        Ok(Statement::Let(let_statement))
    }

    fn parse_return_statement(&mut self) -> Result<Statement<'a>, ParseError<'a>> {
        let return_token = self.current_token;
        self.next_token();

        let value = self.parse_expression()?;
        let return_statement = ReturnStatement::new(return_token, value);

        self.advance_until_current_is(TokenType::Semicolon)?;

        Ok(Statement::Return(return_statement))
    }

    fn curr_token_is(&self, token_type: TokenType) -> bool {
        self.current_token.token_type() == &token_type
    }

    fn peek_token_is(&self, token_type: &TokenType) -> bool {
        self.peek_token.token_type() == token_type
    }

    fn expect_peek(&mut self, token_type: TokenType) -> Result<Token<'a>, ParseError<'a>> {
        if self.peek_token_is(&token_type) {
            self.next_token();
            Ok(self.current_token)
        } else {
            Err(ParseError::UnexpectedToken {
                found: self.peek_token,
                expected: token_type,
            })
        }
    }

    fn advance_until_past(&mut self, token_type: TokenType) -> Result<(), ParseError<'a>> {
        self.advance_until_current_is(token_type)?;
        self.next_token();
        Ok(())
    }

    fn advance_until_current_is(&mut self, token_type: TokenType) -> Result<(), ParseError<'a>> {
        self.advance_until_peek_token_is(token_type)?;
        self.next_token();
        Ok(())
    }

    fn advance_until_peek_token_is(&mut self, token_type: TokenType) -> Result<(), ParseError<'a>> {
        while !self.peek_token_is(&token_type) && !self.peek_token.is_eof() {
            self.next_token();
        }

        if self.peek_token.is_eof() {
            Err(ParseError::UnexpectedEOF {
                expected: token_type,
            })
        } else {
            Ok(())
        }
    }

    fn errors(&self) -> &[ParseError<'a>] {
        &self.errors
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Node, Statement};

    #[test]
    fn test_let_statements() {
        let input = r#"let x = 5;
let y = 10;
let foobar = 838383;"#;

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program().unwrap();
        check_parser_errors(&parser);
        assert_eq!(program.statements.len(), 3);

        let tests = vec!["x", "y", "foobar"];

        for (expected_identifier, statement) in tests.iter().zip(program.statements.iter()) {
            test_let_statement(statement, expected_identifier);
        }
    }

    fn test_let_statement(stmt: &Statement, name: &str) {
        let Statement::Let(let_stmt) = stmt else {
            panic!("expected let statement, got {stmt:?}");
        };

        assert_eq!(let_stmt.token_literal(), "let");
        assert_eq!(let_stmt.name.value, name);
        assert_eq!(let_stmt.name.token_literal(), name);
    }

    #[test]
    fn test_return_statements() {
        let input = r#"return 5;
return 10;
return 993322;"#;

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program().unwrap();
        check_parser_errors(&parser);
        assert_eq!(program.statements.len(), 3);

        let tests = vec!["5", "10", "993322"];

        for (expected_value, statement) in tests.iter().zip(program.statements.iter()) {
            test_return_statement(statement, expected_value);
        }
    }

    fn test_return_statement(stmt: &Statement, value: &str) {
        let Statement::Return(return_stmt) = stmt else {
            panic!("expected return statement, got {stmt:?}");
        };

        assert_eq!(return_stmt.token_literal(), "return");
        // assert_eq!(return_stmt.value.token_literal(), value);
    }

    #[test]
    fn test_string() {}

    #[test]
    fn test_program() {
        let let_stmt = LetStatement::new(
            Token::from_keyword("let"),
            Identifier::new(Token::from_keyword("myVar")),
            Expression::Identifier(Identifier::new(Token::from_keyword("anotherVar"))),
        );
        let program = Program {
            statements: vec![Statement::Let(let_stmt)],
        };

        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }

    fn check_parser_errors(parser: &Parser) {
        if !parser.errors().is_empty() {
            for error in parser.errors() {
                eprintln!("parser error: {error}");
            }

            panic!("parser has errors");
        }
    }
}
