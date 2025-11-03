use crate::ast::{Expression, Identifier, Precedence, Program, Statement};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::fmt::{self, Display};

#[derive(Debug, PartialEq)]
pub enum ParseError<'a> {
    UnexpectedToken {
        found: Token<'a>,
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
            TokenType::Ident => Ok(self.parse_identifier()),
            TokenType::Int => self.parse_integer_literal(),
            TokenType::True | TokenType::False => Ok(self.parse_boolean()),
            TokenType::Bang | TokenType::Minus => self.parse_prefix_expression(),
            _ => Err(ParseError::UnknownPrefixOperator {
                token: self.current_token,
            }),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        let token = self.current_token;

        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(Expression::prefix(token, right))
    }

    fn parse_infix_expression(
        &mut self,
        left: Expression<'a>,
    ) -> Result<Expression<'a>, ParseError<'a>> {
        let token = self.current_token;
        let precedence = self.curr_precedence();

        self.next_token();
        let right = self.parse_expression(precedence)?;

        Ok(Expression::infix(token, left, right))
    }

    fn parse_identifier(&mut self) -> Expression<'a> {
        Expression::ident(self.current_token)
    }

    fn parse_integer_literal(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        let token = self.current_token;
        let value = token
            .literal()
            .parse::<i64>()
            .map_err(|_| ParseError::InvalidInteger)?;

        Ok(Expression::int(token, value))
    }

    fn parse_boolean(&mut self) -> Expression<'a> {
        let token = self.current_token;
        let value = self.curr_token_is(TokenType::True);

        Expression::bool(token, value)
    }

    fn parse_let_statement(&mut self) -> Result<Statement<'a>, ParseError<'a>> {
        let token = self.current_token;
        let name = Identifier::new(self.expect_peek(TokenType::Ident)?);

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
    use crate::ast::{Identifier, Node, Statement};

    #[test]
    fn test_let_statements() {
        let input = r#"let x = 5;
let y = 10;
let foobar = 838383;"#;

        let program = build_program(input);
        assert_eq!(program.statements.len(), 3);

        let tests = vec![("x", "5"), ("y", "10"), ("foobar", "838383")];

        for ((expected_name, expected_value), statement) in
            tests.iter().zip(program.statements.iter())
        {
            test_let_statement(statement, expected_name, expected_value);
        }
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

    #[test]
    fn test_string() {}

    #[test]
    fn test_program() {
        let program = Program::new(vec![Statement::r#let(
            Token::from_keyword("let"),
            Identifier::new(Token::from_keyword("myVar")),
            Expression::ident(Token::from_keyword("anotherVar")),
        )]);

        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let program = build_program(input);

        assert_eq!(program.statements.len(), 1);

        let expression = test_expression(&program.statements[0]);
        test_literal_expression(expression, ExpectedLiteral::String("foobar"));
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let program = build_program(input);
        assert_eq!(program.statements.len(), 1);

        let expression = test_expression(&program.statements[0]);
        test_literal_expression(expression, ExpectedLiteral::Int(5));
    }

    #[test]
    fn test_parse_prefix_expression() {
        use ExpectedLiteral::*;
        let tests = vec![
            ("!5;", "!", Int(5)),
            ("-15;", "-", Int(15)),
            ("!true;", "!", Bool(true)),
            ("!false;", "!", Bool(false)),
        ];

        for (input, operator, expected) in tests {
            let program = build_program(input);
            assert_eq!(program.statements.len(), 1);

            let expression = test_expression(&program.statements[0]);

            let Expression::Prefix {
                operator: op,
                right,
                ..
            } = expression
            else {
                panic!("expected prefix expression, got {:#?}", expression);
            };

            assert_eq!(*op, operator);
            test_literal_expression(right, expected);
        }
    }

    #[test]
    fn test_parse_infix_expression() {
        use ExpectedLiteral::*;
        let tests = vec![
            ("5 + 5;", Int(5), "+", Int(5)),
            ("5 - 5;", Int(5), "-", Int(5)),
            ("5 * 5;", Int(5), "*", Int(5)),
            ("5 / 5;", Int(5), "/", Int(5)),
            ("true == true", Bool(true), "==", Bool(true)),
            ("true != false", Bool(true), "!=", Bool(false)),
            ("false == false", Bool(false), "==", Bool(false)),
        ];

        for (input, left, operator, right) in tests {
            let program = build_program(input);
            assert_eq!(program.statements.len(), 1);

            let expression = test_expression(&program.statements[0]);
            test_infix_expression(expression, left, operator, right);
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
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
        ];

        for (input, expected) in tests {
            let program = build_program(input);
            assert_eq!(program.to_string(), expected);
        }
    }

    // test expression helpers
    fn test_expression<'a>(stmt: &'a Statement<'a>) -> &'a Expression<'a> {
        let Statement::Expression { expression, .. } = stmt else {
            panic!("expected expression statement, got {:#?}", stmt);
        };

        &expression
    }

    enum ExpectedLiteral {
        Int(i64),
        String(&'static str),
        Bool(bool),
    }

    fn test_literal_expression(expression: &Expression, expected: ExpectedLiteral) {
        match expected {
            ExpectedLiteral::Int(v) => test_integer_literal(expression, v),
            ExpectedLiteral::String(s) => test_identifier(expression, s),
            ExpectedLiteral::Bool(b) => test_boolean_expression(expression, b),
        }
    }

    fn test_integer_literal(expression: &Expression, value: i64) {
        let Expression::Int {
            value: expr_value, ..
        } = expression
        else {
            panic!("expected integer expression, got {:#?}", expression);
        };

        assert_eq!(*expr_value, value);
    }

    fn test_identifier(expression: &Expression, value: &str) {
        let Expression::Ident {
            token,
            value: ident_value,
        } = expression
        else {
            panic!("expected identifier expression, got {:#?}", expression);
        };

        assert_eq!(token.literal(), value);
        assert_eq!(ident_value, &value);
    }

    fn test_boolean_expression(expression: &Expression, value: bool) {
        let Expression::Bool {
            value: expr_value, ..
        } = expression
        else {
            panic!("expected boolean expression, got {:#?}", expression);
        };

        assert_eq!(*expr_value, value);
        assert_eq!(
            expression.token_literal(),
            if value { "true" } else { "false" }
        )
    }
    fn test_infix_expression(
        expression: &Expression,
        left: ExpectedLiteral,
        operator: &str,
        right: ExpectedLiteral,
    ) {
        let Expression::Infix {
            left: infix_left,
            operator: infix_op,
            right: infix_right,
            ..
        } = expression
        else {
            panic!("expected infix expression, got {:#?}", expression);
        };

        test_literal_expression(infix_left, left);
        assert_eq!(infix_op, &operator);
        test_literal_expression(infix_right, right);
    }

    // test statement helpers
    fn test_return_statement(stmt: &Statement, value: &str) {
        let Statement::Return {
            value: return_value,
            ..
        } = stmt
        else {
            panic!("expected return statement, got {stmt:?}");
        };

        assert_eq!(stmt.token_literal(), "return");
        assert_eq!(return_value.token_literal(), value);
    }

    fn test_let_statement(stmt: &Statement, name: &str, value: &str) {
        let Statement::Let {
            name: stmt_name,
            value: stmt_value,
            ..
        } = stmt
        else {
            panic!("expected let statement, got {stmt:?}");
        };

        assert_eq!(stmt.token_literal(), "let");
        assert_eq!(stmt_name.token_literal(), name);
        assert_eq!(stmt_value.token_literal(), value);
    }

    // test program helpers
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
