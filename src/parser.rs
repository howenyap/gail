use crate::ast::{Expression, Precedence, Program, Statement};
use crate::error::ParseError;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token<'a>,
    peek_token: Token<'a>,
    errors: Vec<ParseError>,
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

    pub fn parse_program(&mut self) -> Program {
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

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.curr_token_type() {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Ok(Statement::expression(expression))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
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

    fn parse_prefix(&mut self) -> Result<Expression, ParseError> {
        use TokenType::*;

        match self.curr_token_type() {
            Ident => Ok(self.parse_identifier()),
            Int => self.parse_integer_literal(),
            String => Ok(self.parse_string_literal()),
            True | False => Ok(self.parse_boolean()),
            Bang | Minus => self.parse_prefix_expression(),
            Lparen => self.parse_grouped_expression(),
            Lbracket => self.parse_array_expression(),
            If => self.parse_if_expression(),
            Function => self.parse_function_literal(),
            Lbrace => self.parse_hash_literal(),
            _ => Err(ParseError::UnknownPrefixOperator {
                operator: self.current_token.literal().to_string(),
            }),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParseError> {
        let operator = self.current_token.literal().to_string();
        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(Expression::prefix(operator, right))
    }

    fn parse_infix(&mut self, left: Expression) -> Result<Expression, ParseError> {
        use TokenType::*;

        match self.curr_token_type() {
            Plus | Minus | Asterisk | Slash | Equal | NotEqual | GreaterThan | LessThan
            | GreaterThanOrEqual | LessThanOrEqual => self.parse_infix_expression(left),
            Lparen => self.parse_call_expression(left),
            Lbracket => self.parse_index_expression(left),
            _ => Err(ParseError::UnknownInfixOperator {
                operator: self.current_token.literal().to_string(),
            }),
        }
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParseError> {
        let operator = self.current_token.literal().to_string();
        let precedence = self.curr_precedence();

        self.next_token();
        let right = self.parse_expression(precedence)?;

        Ok(Expression::infix(operator, left, right))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParseError> {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenType::Rparen)?;

        Ok(expression)
    }

    fn parse_array_expression(&mut self) -> Result<Expression, ParseError> {
        let mut elements = vec![];

        self.next_token();

        if self.curr_token_is(TokenType::Rbracket) {
            return Ok(Expression::array(elements));
        }

        elements.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            elements.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(TokenType::Rbracket)?;

        Ok(Expression::array(elements))
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParseError> {
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

        Ok(Expression::r#if(condition, consequence, alternative))
    }

    fn parse_function_literal(&mut self) -> Result<Expression, ParseError> {
        self.expect_peek(TokenType::Lparen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(TokenType::Lbrace)?;

        let body = self.parse_block_statement()?;

        Ok(Expression::function(parameters, body))
    }

    fn parse_hash_literal(&mut self) -> Result<Expression, ParseError> {
        let mut pairs = vec![];

        while !self.peek_token_is(TokenType::Rbrace) {
            self.next_token();

            let key = self.parse_expression(Precedence::Lowest)?;
            if !key.hashable() {
                return Err(ParseError::InvalidHashKey {
                    key: key.to_string(),
                });
            }

            self.expect_peek(TokenType::Colon)?;
            self.next_token();

            let value = self.parse_expression(Precedence::Lowest)?;

            if self.peek_token_is(TokenType::Comma) {
                self.next_token();
            }

            pairs.push((key, value));
        }

        self.expect_peek(TokenType::Rbrace)?;

        Ok(Expression::hashmap(pairs))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Expression>, ParseError> {
        let mut identifiers = vec![];

        if self.peek_token_is(TokenType::Rparen) {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();
        let identifer = self.current_token.literal().to_string();
        identifiers.push(Expression::ident(identifer));

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();

            let identifer = self.current_token.literal().to_string();
            let ident = Expression::ident(identifer);
            identifiers.push(ident);
        }

        self.expect_peek(TokenType::Rparen)?;

        Ok(identifiers)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Result<Expression, ParseError> {
        let arguments = self.parse_call_arguments()?;

        Ok(Expression::call(function, arguments))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<Expression, ParseError> {
        self.next_token();
        let index = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenType::Rbracket)?;

        Ok(Expression::index(left, index))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, ParseError> {
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

    fn parse_identifier(&mut self) -> Expression {
        Expression::ident(self.current_token.literal().to_string())
    }

    fn parse_integer_literal(&mut self) -> Result<Expression, ParseError> {
        let value = self
            .current_token
            .literal()
            .parse::<i64>()
            .map_err(|_| ParseError::InvalidInteger)?;

        Ok(Expression::int(value))
    }

    fn parse_boolean(&mut self) -> Expression {
        Expression::bool(self.current_token.bool())
    }

    fn parse_string_literal(&mut self) -> Expression {
        Expression::string(self.current_token.literal().to_string())
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect_peek(TokenType::Ident)?;
        let name = Expression::ident(self.current_token.literal().to_string());

        self.expect_peek(TokenType::Assign)?;
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Ok(Statement::r#let(name, value))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Ok(Statement::r#return(value))
    }

    fn parse_block_statement(&mut self) -> Result<Expression, ParseError> {
        let _token = self.current_token;
        let mut statements = vec![];
        self.next_token();

        while !self.curr_token_is(TokenType::Rbrace) && !self.current_token.is_eof() {
            statements.push(self.parse_statement()?);
            self.next_token();
        }

        Ok(Expression::block(statements))
    }

    fn curr_token_is(&self, token_type: TokenType) -> bool {
        self.curr_token_type() == &token_type
    }

    fn peek_token_is(&self, token_type: TokenType) -> bool {
        self.peek_token_type() == &token_type
    }

    fn expect_peek(&mut self, token_type: TokenType) -> Result<(), ParseError> {
        if self.peek_token_is(token_type) {
            self.next_token();
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                found: self.peek_token.literal().to_string(),
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
            Plus | Minus
                | Asterisk
                | Slash
                | Equal
                | NotEqual
                | LessThan
                | GreaterThan
                | LessThanOrEqual
                | GreaterThanOrEqual
                | Lparen
                | Lbracket
        )
    }

    pub fn errors(&self) -> Option<Vec<ParseError>> {
        if !self.errors.is_empty() {
            Some(self.errors.clone())
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Statement;
    use crate::utils::join_with_separator;

    #[test]
    fn test_let_statements() {
        use ExpectedLiteral::*;

        let tests = vec![
            ("let x = 5;", "x", Int(5)),
            ("let y = true;", "y", Bool(true)),
            ("let foobar = y", "foobar", Ident("y")),
        ];

        for (input, expected_identifier, expected_value) in tests {
            let program = build_program(input);
            let statements: Vec<_> = program.statements().collect();
            assert_eq!(1, statements.len());

            let Statement::Let { name, value, .. } = &statements[0] else {
                panic!("expected let statement, got {:#?}", statements[0]);
            };

            assert_eq!(expected_identifier, name.to_string());
            test_literal_expression(expected_value, value);
        }
    }

    #[test]
    fn test_return_statements() {
        use ExpectedLiteral::*;
        let tests = vec![
            ("return 5", Int(5)),
            ("return false", Bool(false)),
            ("return x", Ident("x")),
        ];

        for (input, expected_value) in tests {
            let program = build_program(input);
            let statements: Vec<_> = program.statements().collect();
            assert_eq!(1, statements.len());

            let Statement::Return { value } = &statements[0] else {
                panic!("expected return statement, got {:#?}", statements[0]);
            };

            test_literal_expression(expected_value, value);
        }
    }
    #[test]
    fn test_program() {
        let my_var = Token::from_keyword("myVar");
        let another_var = Token::from_keyword("anotherVar");

        let statement = Statement::r#let(
            Expression::ident(my_var.literal().to_string()),
            Expression::ident(another_var.literal().to_string()),
        );
        let program = Program::new(vec![statement]);

        assert_eq!("let myVar = anotherVar;", program.to_string());
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let program = build_program(input);

        let statements: Vec<_> = program.statements().collect();
        assert_eq!(1, statements.len());

        let expression = expect_expression(&statements[0]);
        test_literal_expression(ExpectedLiteral::Ident("foobar"), expression);
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";
        let program = build_program(input);
        let statements: Vec<_> = program.statements().collect();
        assert_eq!(1, statements.len());

        let expression = expect_expression(&statements[0]);
        test_literal_expression(ExpectedLiteral::Int(5), expression);
    }

    #[test]
    fn test_string_literal_expression() {
        let input = "\"fast, reliable, productive.\";";
        let program = build_program(input);
        let statements: Vec<_> = program.statements().collect();
        assert_eq!(1, statements.len());

        let expression = expect_expression(&statements[0]);
        test_literal_expression(
            ExpectedLiteral::String("fast, reliable, productive."),
            expression,
        );
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
            let statements: Vec<_> = program.statements().collect();
            assert_eq!(1, statements.len());

            let expression = expect_expression(&statements[0]);

            let Expression::Prefix {
                operator: received_op,
                right: received_right,
                ..
            } = expression
            else {
                panic!("expected prefix expression, got {expression:#?}");
            };

            assert_eq!(operator, *received_op);
            test_literal_expression(expected, received_right);
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
            let statements: Vec<_> = program.statements().collect();
            assert_eq!(1, statements.len());

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
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for (input, expected) in tests {
            let program = build_program(input);
            assert_eq!(expected, program.to_string());
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        let program = build_program(input);
        let statements: Vec<_> = program.statements().collect();
        assert_eq!(1, statements.len());

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
        test_infix_expression(condition, Ident("x"), "<", Ident("y"));

        let statements = expect_block_expression(consequence);
        assert_eq!(1, statements.len());

        let expression = expect_expression(&statements[0]);
        test_literal_expression(ExpectedLiteral::Ident("x"), expression);

        assert!(alternative.is_none());
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
        let program = build_program(input);
        let statements: Vec<_> = program.statements().collect();
        assert_eq!(1, statements.len());

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
        test_infix_expression(condition, Ident("x"), "<", Ident("y"));

        let consequence = consequence.as_ref();
        let consequence_statements = expect_block_expression(consequence);
        assert_eq!(1, consequence_statements.len());
        let consequence_expression = expect_expression(&consequence_statements[0]);
        test_literal_expression(Ident("x"), consequence_expression);

        let Some(alternative) = alternative.as_ref() else {
            panic!("expected alternative block, got None");
        };

        let alternative_statements = expect_block_expression(alternative);
        assert_eq!(1, alternative_statements.len());
        let alternative_expression = expect_expression(&alternative_statements[0]);
        test_literal_expression(Ident("y"), alternative_expression);
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) {x + y; }";
        let program = build_program(input);
        let statements: Vec<_> = program.statements().collect();
        assert_eq!(1, statements.len());

        let statement = expect_expression(&statements[0]);

        let Expression::Function {
            parameters, body, ..
        } = statement
        else {
            panic!("expected function expression, got {statement:#}");
        };

        use ExpectedLiteral::*;

        assert_eq!(2, parameters.len());
        test_literal_expression(Ident("x"), &parameters[0]);
        test_literal_expression(Ident("y"), &parameters[1]);

        let body_statements = expect_block_expression(body);
        assert_eq!(1, body_statements.len());
        let body_expression = expect_expression(&body_statements[0]);
        test_infix_expression(body_expression, Ident("x"), "+", Ident("y"));
    }

    #[test]
    fn test_array_literal() {
        let input = "[1, 2 * 2, 3 + 3]";
        let program = build_program(input);
        let statements: Vec<_> = program.statements().collect();
        assert_eq!(1, statements.len());

        let statement = expect_expression(&statements[0]);
        let Expression::Array { elements } = statement else {
            panic!("expected array expression, got {statement:#}");
        };

        assert_eq!(3, elements.len());
        test_integer_literal(&elements[0], 1);
        test_infix_expression(
            &elements[1],
            ExpectedLiteral::Int(2),
            "*",
            ExpectedLiteral::Int(2),
        );
        test_infix_expression(
            &elements[2],
            ExpectedLiteral::Int(3),
            "+",
            ExpectedLiteral::Int(3),
        );
    }

    #[test]
    fn test_parse_hash_literal() {
        use ExpectedLiteral::*;

        let input = "{\"one\": 1, \"two\": 2, \"three\": 3}";
        let program = build_program(input);
        let statements: Vec<_> = program.statements().collect();
        assert_eq!(1, statements.len());

        let statement = expect_expression(&statements[0]);

        let expected_pairs = vec![
            (String("one"), Int(1)),
            (String("two"), Int(2)),
            (String("three"), Int(3)),
        ];
        let pairs = expect_hash_expression(statement);
        assert_eq!(expected_pairs.len(), pairs.len());

        for ((expected_key, expected_value), (key, value)) in
            expected_pairs.into_iter().zip(pairs.into_iter())
        {
            test_literal_expression(expected_key, key);
            test_literal_expression(expected_value, value);
        }
    }

    #[test]
    fn test_parse_empty_hash_literal() {
        let input = "{}";
        let program = build_program(input);
        let statements: Vec<_> = program.statements().collect();
        assert_eq!(1, statements.len());

        let statement = expect_expression(&statements[0]);
        let pairs = expect_hash_expression(statement);
        assert_eq!(0, pairs.len());
    }

    #[test]
    fn test_parse_hash_literal_with_expressions() {
        use ExpectedLiteral::*;

        let input = "{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5}";
        let program = build_program(input);
        let statements: Vec<_> = program.statements().collect();
        assert_eq!(1, statements.len());

        let statement = expect_expression(&statements[0]);
        let expected_pairs = vec![
            (String("one"), (Int(0), "+", Int(1))),
            (String("two"), (Int(10), "-", Int(8))),
            (String("three"), (Int(15), "/", Int(5))),
        ];

        let pairs = expect_hash_expression(statement);
        assert_eq!(expected_pairs.len(), pairs.len());

        for ((expected_key, expected_value), (key, value)) in
            expected_pairs.into_iter().zip(pairs.into_iter())
        {
            let (left, operator, right) = expected_value;

            test_literal_expression(expected_key, key);
            test_infix_expression(value, left, operator, right);
        }
    }

    #[test]
    fn test_unhashable_keys() {
        let input = r#"
        {[1, 2, 3]: 4}; 
        { 5: 6 }: 7;
        fn(x) { x; }: 8;
        "#;

        let expected = vec![
            "invalid hash key: [1, 2, 3], only integers, booleans, and strings can be used as keys",
            "invalid hash key: 5, only integers, booleans, and strings can be used as keys",
            "invalid hash key: fn(x) { x; }, only integers, booleans, and strings can be used as keys",
        ];

        test_error(input, expected);
    }

    #[test]
    fn test_index_expression() {
        let input = "myArray[1 + 1]";
        let program = build_program(input);
        let statements: Vec<_> = program.statements().collect();
        assert_eq!(1, statements.len());

        let statement = expect_expression(&statements[0]);
        let Expression::Index {
            left: received_left,
            index: received_index,
        } = statement
        else {
            panic!("expected index expression, got {statement:#}");
        };

        test_identifier(received_left, "myArray");
        test_infix_expression(
            received_index,
            ExpectedLiteral::Int(1),
            "+",
            ExpectedLiteral::Int(1),
        );
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
            let statements: Vec<_> = program.statements().collect();
            assert_eq!(1, statements.len());

            let statement = expect_expression(&statements[0]);

            let Expression::Function { parameters, .. } = statement else {
                panic!("expected function expression, got {statement:#}");
            };

            assert_eq!(expected.len(), parameters.len());

            for i in 0..expected.len() {
                test_literal_expression(ExpectedLiteral::Ident(expected[i]), &parameters[i]);
            }
        }
    }

    #[test]
    fn text_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5)";
        let program = build_program(input);
        let statements: Vec<_> = program.statements().collect();
        assert_eq!(1, statements.len());

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
        test_literal_expression(Ident("add"), function);
        assert_eq!(3, arguments.len());

        test_literal_expression(ExpectedLiteral::Int(1), &arguments[0]);
        test_infix_expression(&arguments[1], Int(2), "*", Int(3));
        test_infix_expression(&arguments[2], Int(4), "+", Int(5));
    }

    // test expression helpers
    fn expect_expression(statement: &Statement) -> &Expression {
        let Statement::Expression { expression, .. } = statement else {
            panic!("expected expression statement, got {statement:#?}");
        };

        expression
    }

    enum ExpectedLiteral {
        Int(i64),
        Ident(&'static str),
        Bool(bool),
        String(&'static str),
    }

    fn test_literal_expression(expected: ExpectedLiteral, expression: &Expression) {
        use ExpectedLiteral::*;

        match expected {
            Int(v) => test_integer_literal(expression, v),
            Ident(s) => test_identifier(expression, s),
            Bool(b) => test_boolean_expression(expression, b),
            String(s) => test_string_literal(expression, s),
        }
    }

    fn test_integer_literal(expression: &Expression, value: i64) {
        let Expression::Int {
            value: expr_value, ..
        } = expression
        else {
            panic!("expected integer expression, got {expression:#?}");
        };

        assert_eq!(value, *expr_value);
    }

    fn test_string_literal(expression: &Expression, value: &str) {
        let Expression::String { value: expr_value } = expression else {
            panic!("expected string expression, got {expression:#?}");
        };

        assert_eq!(value, expr_value);
    }

    fn test_identifier(expression: &Expression, value: &str) {
        let Expression::Ident { value: ident_value } = expression else {
            panic!("expected identifier expression, got {expression:#?}");
        };

        assert_eq!(&value, ident_value);
    }

    fn test_boolean_expression(expression: &Expression, value: bool) {
        let Expression::Bool {
            value: expr_value, ..
        } = expression
        else {
            panic!("expected boolean expression, got {expression:#?}");
        };

        assert_eq!(value, *expr_value);
    }

    fn test_infix_expression(
        expression: &Expression,
        expected_left: ExpectedLiteral,
        operator: &str,
        expected_right: ExpectedLiteral,
    ) {
        let Expression::Infix {
            left: received_left,
            operator: received_op,
            right: received_right,
            ..
        } = expression
        else {
            panic!("expected infix expression, got {expression:#?}");
        };

        test_literal_expression(expected_left, received_left);
        assert_eq!(&operator, received_op);
        test_literal_expression(expected_right, received_right);
    }

    fn expect_block_expression(expression: &Expression) -> &[Statement] {
        let Expression::Block { statements } = expression else {
            panic!("expected block expression, got {expression:#?}");
        };

        statements
    }

    fn expect_hash_expression(expression: &Expression) -> &Vec<(Expression, Expression)> {
        let Expression::HashMap { pairs } = expression else {
            panic!("expected hash expression, got {expression:#?}");
        };

        pairs
    }

    fn build_program(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if let Some(errors) = parser.errors() {
            eprintln!("{}", join_with_separator(&errors, "\n"));
            panic!("parser has errors");
        }

        program
    }

    fn test_error(input: &str, expected: Vec<&str>) {
        let lexer = Lexer::new(input);
        let parser = Parser::new(lexer);

        if let Some(errors) = parser.errors() {
            assert_eq!(expected.len(), errors.len());

            for (expected, error) in expected.into_iter().zip(errors.iter()) {
                let received = error.to_string();
                assert_eq!(expected, received);
            }
        }
    }
}
