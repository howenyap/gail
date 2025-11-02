use crate::ast::{
    Expression, ExpressionStatement, Identifier, InfixExpression, IntegerLiteral, LetStatement,
    Precedence, PrefixExpression, Program, ReturnStatement, Statement,
};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
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
    InvalidInteger,
    UnknownPrefixOperator {
        token: Token<'a>,
    },
}

impl<'a> Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken { found, expected } => {
                write!(
                    f,
                    "expected next token to be {expected:?}, got {found} instead"
                )
            }
            ParseError::UnknownStatement { token } => {
                write!(f, "unknown statement: {token}")
            }
            ParseError::UnexpectedEOF { expected } => {
                write!(
                    f,
                    "expected token to be {expected:?} but reached EOF instead"
                )
            }
            ParseError::InvalidInteger => write!(f, "invalid integer"),
            ParseError::UnknownPrefixOperator { token } => {
                write!(f, "unknown prefix operator: {token}")
            }
        }
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

    pub fn parse_program(&mut self) -> Program<'a> {
        let mut statements = vec![];

        while !self.current_token.is_eof() {
            match self.parse_statement() {
                Ok(statement) => statements.push(statement),
                Err(error) => self.errors.push(error),
            }
            self.next_token();
        }

        Program::new(statements)
    }

    fn parse_statement(&mut self) -> Result<Statement<'a>, ParseError<'a>> {
        match self.curr_token_type() {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement<'a>, ParseError<'a>> {
        let token = self.current_token;
        let expression = self.parse_expression(Precedence::Lowest)?;

        self.advance_until(TokenType::Semicolon);

        Ok(Statement::expression(token, expression))
    }

    fn parse_expression(
        &mut self,
        precedence: Precedence,
    ) -> Result<Expression<'a>, ParseError<'a>> {
        let mut left = self.parse_prefix()?;

        while !self.peek_token_is(&TokenType::Semicolon) && precedence < self.peek_precedence() {
            if !self.is_infix_operator(self.peek_token_type()) {
                return Ok(left);
            }
            
            self.next_token();
            left = self.parse_infix_expression(left)?;
        }

        Ok(left)
    }

    fn parse_prefix(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        match self.curr_token_type() {
            TokenType::Ident => self.parse_identifier(),
            TokenType::Int => self.parse_integer_literal(),
            TokenType::Bang | TokenType::Minus => self.parse_prefix_expression(),
            _ => Err(ParseError::UnknownPrefixOperator {
                token: self.current_token,
            }),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        let token = self.current_token;
        let operator = token.literal;

        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(Expression::prefix(token, operator, right))
    }

    fn parse_infix_expression(
        &mut self,
        left: Expression<'a>,
    ) -> Result<Expression<'a>, ParseError<'a>> {
        let token = self.current_token;
        let operator = token.literal;
        let precedence = self.curr_precedence();

        self.next_token();
        let right = self.parse_expression(precedence)?;

        Ok(Expression::infix(token, left, operator, right))
    }

    fn parse_identifier(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        Ok(Expression::identifier(self.current_token))
    }

    fn parse_integer_literal(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        let token = self.current_token;
        let value = token
            .literal()
            .parse::<i64>()
            .map_err(|_| ParseError::InvalidInteger)?;

        Ok(Expression::integer(token, value))
    }

    fn parse_let_statement(&mut self) -> Result<Statement<'a>, ParseError<'a>> {
        let token = self.current_token;

        let ident_token = self.expect_peek(TokenType::Ident)?;
        let name = Identifier::new(ident_token);

        self.expect_peek(TokenType::Assign)?;
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        self.advance_until(TokenType::Semicolon);

        Ok(Statement::r#let(token, name, value))
    }

    fn parse_return_statement(&mut self) -> Result<Statement<'a>, ParseError<'a>> {
        let token = self.current_token;
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        self.advance_until(TokenType::Semicolon);

        Ok(Statement::r#return(token, value))
    }

    fn curr_token_is(&self, token_type: TokenType) -> bool {
        self.curr_token_type() == &token_type
    }

    fn peek_token_is(&self, token_type: &TokenType) -> bool {
        self.peek_token_type() == token_type
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

    fn curr_token_type(&self) -> &TokenType {
        self.current_token.token_type()
    }

    fn peek_token_type(&self) -> &TokenType {
        self.peek_token.token_type()
    }

    fn curr_precedence(&self) -> Precedence {
        self.current_token.precedence()
    }

    fn peek_precedence(&self) -> Precedence {
        self.peek_token.precedence()
    }

    fn advance_until(&mut self, token_type: TokenType) {
        while !self.curr_token_is(token_type) && !self.peek_token.is_eof() {
            self.next_token();
        }
    }

    fn is_infix_operator(&self, token_type: &TokenType) -> bool {
        matches!(
            token_type,
            TokenType::Plus
                | TokenType::Minus
                | TokenType::Asterisk
                | TokenType::Slash
                | TokenType::Equal
                | TokenType::NotEqual
                | TokenType::LessThan
                | TokenType::GreaterThan
        )
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

        let program = build_program(input);
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

        let program = build_program(input);
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
        assert_eq!(return_stmt.value.token_literal(), value);
    }

    #[test]
    fn test_string() {}

    #[test]
    fn test_program() {
        let program = Program::new(vec![Statement::r#let(
            Token::from_keyword("let"),
            Identifier::new(Token::from_keyword("myVar")),
            Expression::Identifier(Identifier::new(Token::from_keyword("anotherVar"))),
        )]);

        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let program = build_program(input);

        assert_eq!(program.statements.len(), 1);

        let Statement::Expression(expr) = &program.statements[0] else {
            panic!(
                "expected expression statement, got {:#?}",
                program.statements[0]
            );
        };

        let Expression::Identifier(ident) = &expr.expression else {
            panic!("expected identifier expression, got {:#?}", expr.expression);
        };

        assert_eq!(ident.value, "foobar");
        assert_eq!(ident.token_literal(), "foobar");
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let program = build_program(input);

        assert_eq!(program.statements.len(), 1);
        let Statement::Expression(expr) = &program.statements[0] else {
            panic!(
                "expected expression statement, got {:#?}",
                program.statements[0]
            );
        };

        let Expression::Integer(integer) = &expr.expression else {
            panic!("expected integer expression, got {:#?}", expr.expression);
        };

        assert_eq!(integer.value, 5);
    }

    #[test]
    fn test_parse_prefix_expression() {
        let tests = vec![("!5;", "!", 5), ("-15;", "-", 15)];
        for (input, operator, value) in tests {
            let program = build_program(input);
            assert_eq!(program.statements.len(), 1);

            let Statement::Expression(expr) = &program.statements[0] else {
                panic!(
                    "expected expression statement, got {:#?}",
                    program.statements[0]
                );
            };

            let Expression::Prefix(prefix) = &expr.expression else {
                panic!("expected prefix expression, got {:#?}", expr.expression);
            };

            assert_eq!(prefix.operator, operator);
            test_integer_literal(&prefix.right, value);
        }
    }

    #[test]
    fn test_parse_infix_expression() {
        let tests = vec![
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
        ];

        for (input, left, operator, right) in tests {
            let program = build_program(input);
            assert_eq!(program.statements.len(), 1);

            let Statement::Expression(expr) = &program.statements[0] else {
                panic!(
                    "expected expression statement, got {:#?}",
                    program.statements[0]
                );
            };

            let Expression::Infix(infix) = &expr.expression else {
                panic!("expected infix expression, got {:#?}", expr.expression);
            };

            test_integer_literal(&infix.left, left);
            assert_eq!(infix.operator, operator);
            test_integer_literal(&infix.right, right);
        }
    }

    #[test]
    fn test_parse_operator_precedence() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4;", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4;", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5;",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for (input, expected) in tests {
            let program = build_program(input);
            assert_eq!(program.to_string(), expected);
        }
    }

    fn test_integer_literal(expression: &Expression, value: i64) {
        let Expression::Integer(integer) = expression else {
            panic!("expected integer expression, got {:#?}", expression);
        };

        assert_eq!(integer.value, value);
    }

    fn build_program(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parser_errors(&parser);
        program
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
