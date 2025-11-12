use crate::ast::{Expression, Precedence, Program, Statement};
use crate::error::ParseError;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token<'a>,
    peek_token: Token<'a>,
    errors: Vec<ParseError<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();
        let errors = vec![];

        Self {
            lexer,
            current_token,
            peek_token,
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

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Ok(Statement::expression(token, expression))
    }

    fn parse_expression(
        &mut self,
        precedence: Precedence,
    ) -> Result<Expression<'a>, ParseError<'a>> {
        let mut left = self.parse_prefix()?;

        while !self.peek_token_is(TokenType::Semicolon) && precedence < self.peek_precedence() {
            if !self.is_infix_operator(self.peek_token_type()) {
                return Ok(left);
            }

            self.next_token();
            left = self.parse_infix(left)?;
        }

        Ok(left)
    }

    fn parse_prefix(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        use TokenType::*;

        match self.curr_token_type() {
            Ident => Ok(self.parse_identifier()),
            Int => self.parse_integer_literal(),
            True | False => Ok(self.parse_boolean()),
            Bang | Minus => self.parse_prefix_expression(),
            Lparen => self.parse_grouped_expression(),
            If => self.parse_if_expression(),
            Function => self.parse_function_literal(),
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

    fn parse_infix(&mut self, left: Expression<'a>) -> Result<Expression<'a>, ParseError<'a>> {
        match self.curr_token_type() {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Asterisk
            | TokenType::Slash
            | TokenType::Equal
            | TokenType::NotEqual
            | TokenType::GreaterThan
            | TokenType::LessThan => self.parse_infix_expression(left),
            TokenType::Lparen => self.parse_call_expression(left),
            _ => Err(ParseError::UnknownInfixOperator {
                token: self.current_token,
            }),
        }
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

    fn parse_grouped_expression(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenType::Rparen)?;

        Ok(exp)
    }

    fn parse_if_expression(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        let token = self.current_token;

        self.expect_peek(TokenType::Lparen)?;
        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenType::Rparen)?;
        self.expect_peek(TokenType::Lbrace)?;
        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token_is(TokenType::Else) {
            self.next_token();
            self.expect_peek(TokenType::Lbrace)?;
            Some(self.parse_block_statement()?)
        } else {
            None
        };

        Ok(Expression::r#if(token, condition, consequence, alternative))
    }

    fn parse_function_literal(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        let token = self.current_token;

        self.expect_peek(TokenType::Lparen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(TokenType::Lbrace)?;

        let body = self.parse_block_statement()?;

        Ok(Expression::function(token, parameters, body))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Expression<'a>>, ParseError<'a>> {
        let mut identifiers = vec![];

        if self.peek_token_is(TokenType::Rparen) {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();
        identifiers.push(Expression::ident(self.current_token));

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();

            let ident = Expression::ident(self.current_token);
            identifiers.push(ident);
        }

        self.expect_peek(TokenType::Rparen)?;

        Ok(identifiers)
    }

    fn parse_call_expression(
        &mut self,
        function: Expression<'a>,
    ) -> Result<Expression<'a>, ParseError<'a>> {
        let token = self.current_token;
        let arguments = self.parse_call_arguments()?;

        Ok(Expression::call(token, function, arguments))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression<'a>>, ParseError<'a>> {
        let mut args = vec![];

        if self.peek_token_is(TokenType::Rparen) {
            self.next_token();
            return Ok(args);
        }

        self.next_token();
        args.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(TokenType::Rparen)?;

        Ok(args)
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

        self.expect_peek(TokenType::Ident)?;
        let name = Expression::ident(self.current_token);

        self.expect_peek(TokenType::Assign)?;
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Ok(Statement::r#let(token, name, value))
    }

    fn parse_return_statement(&mut self) -> Result<Statement<'a>, ParseError<'a>> {
        let token = self.current_token;
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Ok(Statement::r#return(token, value))
    }

    fn parse_block_statement(&mut self) -> Result<Expression<'a>, ParseError<'a>> {
        let token = self.current_token;
        let mut statements = vec![];
        self.next_token();

        while !self.curr_token_is(TokenType::Rbrace) && !self.current_token.is_eof() {
            statements.push(self.parse_statement()?);
            self.next_token();
        }

        Ok(Expression::block(token, statements))
    }

    fn curr_token_is(&self, token_type: TokenType) -> bool {
        self.curr_token_type() == &token_type
    }

    fn peek_token_is(&self, token_type: TokenType) -> bool {
        self.peek_token_type() == &token_type
    }

    fn expect_peek(&mut self, token_type: TokenType) -> Result<(), ParseError<'a>> {
        if self.peek_token_is(token_type) {
            self.next_token();
            Ok(())
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

    fn is_infix_operator(&self, token_type: &TokenType) -> bool {
        use TokenType::*;

        matches!(
            token_type,
            Plus | Minus | Asterisk | Slash | Equal | NotEqual | LessThan | GreaterThan | Lparen
        )
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn print_errors(&self) {
        for error in &self.errors {
            eprintln!("parser error: {error}");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Statement;

    #[test]
    fn test_let_statements() {
        use ExpectedLiteral::*;

        let tests = vec![
            ("let x = 5;", "x", Int(5)),
            ("let y = true;", "y", Bool(true)),
            ("let foobar = y", "foobar", String("y")),
        ];

        for (input, expected_identifier, expected_value) in tests {
            let program = build_program(input);
            let statements = program.statements();
            assert_eq!(statements.len(), 1);

            let Statement::Let { token, name, value } = &statements[0] else {
                panic!("expected let statement, got {:#?}", statements[0]);
            };

            assert_eq!(token.literal(), "let");
            assert_eq!(name.token_literal(), expected_identifier);
            test_literal_expression(value, expected_value);
        }
    }

    #[test]
    fn test_return_statements() {
        use ExpectedLiteral::*;
        let tests = vec![
            ("return 5", Int(5)),
            ("return false", Bool(false)),
            ("return x", String("x")),
        ];

        for (input, expected_value) in tests {
            let program = build_program(input);
            let statements = program.statements();
            assert_eq!(statements.len(), 1);

            let Statement::Return { token, value } = &statements[0] else {
                panic!("expected return statement, got {:#?}", statements[0]);
            };

            assert_eq!(token.literal(), "return");
            test_literal_expression(value, expected_value);
        }
    }
    #[test]
    fn test_program() {
        let statement = Statement::r#let(
            Token::from_keyword("let"),
            Expression::ident(Token::from_keyword("myVar")),
            Expression::ident(Token::from_keyword("anotherVar")),
        );
        let program = Program::new(vec![statement]);

        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let program = build_program(input);

        let statements = program.statements();
        assert_eq!(statements.len(), 1);

        let expression = expect_expression(&statements[0]);
        test_literal_expression(expression, ExpectedLiteral::String("foobar"));
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let program = build_program(input);
        let statements = program.statements();
        assert_eq!(statements.len(), 1);

        let expression = expect_expression(&statements[0]);
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
            let statements = program.statements();
            assert_eq!(statements.len(), 1);

            let expression = expect_expression(&statements[0]);

            let Expression::Prefix {
                operator: op,
                right,
                ..
            } = expression
            else {
                panic!("expected prefix expression, got {expression:#?}");
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
            let statements = program.statements();
            assert_eq!(statements.len(), 1);

            let expression = expect_expression(&statements[0]);
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
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];

        for (input, expected) in tests {
            let program = build_program(input);
            assert_eq!(program.to_string(), expected);
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let program = build_program(input);
        let statements = program.statements();
        assert_eq!(statements.len(), 1);

        let expression = expect_expression(&statements[0]);
        let Expression::If {
            condition,
            consequence,
            alternative,
            ..
        } = expression
        else {
            panic!("expected if expression, got {expression:#?}")
        };

        use ExpectedLiteral::*;
        test_infix_expression(condition, String("x"), "<", String("y"));

        let statements = expect_block_expression(consequence);
        assert_eq!(statements.len(), 1);

        let expression = expect_expression(&statements[0]);
        test_literal_expression(expression, String("x"));

        assert!(alternative.is_none());
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
        let program = build_program(input);
        let statements = program.statements();
        assert_eq!(statements.len(), 1);

        let expression = expect_expression(&statements[0]);
        let Expression::If {
            condition,
            consequence,
            alternative,
            ..
        } = expression
        else {
            panic!("expected if expression, got {expression:#?}")
        };

        use ExpectedLiteral::*;
        test_infix_expression(condition, String("x"), "<", String("y"));

        let consequence = consequence.as_ref();
        let consequence_statements = expect_block_expression(consequence);
        assert_eq!(consequence_statements.len(), 1);
        let consequence_expression = expect_expression(&consequence_statements[0]);
        test_literal_expression(consequence_expression, String("x"));

        let Some(alternative) = alternative.as_ref() else {
            panic!("expected alternative block, got None");
        };

        let alternative_statements = expect_block_expression(alternative);
        assert_eq!(alternative_statements.len(), 1);
        let alternative_expression = expect_expression(&alternative_statements[0]);
        test_literal_expression(alternative_expression, String("y"));
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) {x + y; }";
        let program = build_program(input);
        let statements = program.statements();
        assert_eq!(statements.len(), 1);

        let statement = expect_expression(&statements[0]);

        let Expression::Function {
            parameters, body, ..
        } = statement
        else {
            panic!("expected function expression, got {statement:#}");
        };

        use ExpectedLiteral::*;

        assert_eq!(parameters.len(), 2);
        test_literal_expression(&parameters[0], String("x"));
        test_literal_expression(&parameters[1], String("y"));

        let body_statements = expect_block_expression(body);
        assert_eq!(body_statements.len(), 1);
        let body_expression = expect_expression(&body_statements[0]);
        test_infix_expression(body_expression, String("x"), "+", String("y"));
    }

    #[test]
    fn test_function_parameters() {
        let tests = vec![
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
        ];

        for (input, expected) in tests {
            let program = build_program(input);
            let statements = program.statements();
            assert_eq!(statements.len(), 1);

            let statement = expect_expression(&program.statements()[0]);

            let Expression::Function { parameters, .. } = statement else {
                panic!("expected function expression, got {statement:#}");
            };

            assert_eq!(parameters.len(), expected.len());

            for i in 0..expected.len() {
                test_literal_expression(&parameters[i], ExpectedLiteral::String(expected[i]));
            }
        }
    }

    #[test]
    fn text_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5)";
        let program = build_program(input);
        let statements = program.statements();
        assert_eq!(statements.len(), 1);

        let expression = expect_expression(&statements[0]);

        let Expression::Call {
            function,
            arguments,
            ..
        } = expression
        else {
            panic!("expected call expression, got {expression:#?}")
        };

        use ExpectedLiteral::*;
        test_literal_expression(function, String("add"));
        assert_eq!(arguments.len(), 3);

        test_literal_expression(&arguments[0], ExpectedLiteral::Int(1));
        test_infix_expression(&arguments[1], Int(2), "*", Int(3));
        test_infix_expression(&arguments[2], Int(4), "+", Int(5));
    }

    // test expression helpers
    fn expect_expression<'a>(statement: &'a Statement<'a>) -> &'a Expression<'a> {
        let Statement::Expression { expression, .. } = statement else {
            panic!("expected expression statement, got {statement:#?}");
        };

        expression
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
            panic!("expected integer expression, got {expression:#?}");
        };

        assert_eq!(*expr_value, value);
    }

    fn test_identifier(expression: &Expression, value: &str) {
        let Expression::Ident {
            token,
            value: ident_value,
        } = expression
        else {
            panic!("expected identifier expression, got {expression:#?}");
        };

        assert_eq!(token.literal(), value);
        assert_eq!(ident_value, &value);
    }

    fn test_boolean_expression(expression: &Expression, value: bool) {
        let Expression::Bool {
            value: expr_value, ..
        } = expression
        else {
            panic!("expected boolean expression, got {expression:#?}");
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
            panic!("expected infix expression, got {expression:#?}");
        };

        test_literal_expression(infix_left, left);
        assert_eq!(infix_op, &operator);
        test_literal_expression(infix_right, right);
    }

    fn expect_block_expression<'a>(expression: &'a Expression<'a>) -> &'a [Statement<'a>] {
        let Expression::Block { statements, .. } = expression else {
            panic!("expected block expression, got {expression:#?}");
        };

        statements
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
        if parser.has_errors() {
            parser.print_errors();
            panic!("parser has errors");
        }
    }
}
