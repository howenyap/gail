use crate::ast::Node;
use crate::ast::{Expression, Program, Statement};
use crate::error::EvalError;
use crate::object::{Object, ObjectTrait};

pub struct Evaluator;

impl Evaluator {
    pub fn eval(node: &Node) -> Result<Object> {
        let evaluated = match node {
            Node::Expression(expr) => Self::eval_expression(expr)?,
            Node::Statement(stmt) => Self::eval_statement(stmt)?,
            Node::Program(prog) => Self::eval_program(prog)?,
        };

        Ok(evaluated)
    }

    fn eval_expression(expr: &Expression) -> Result<Object> {
        let evaluated = match expr {
            Expression::Int { value, .. } => Object::Integer(*value),
            Expression::Bool { value, .. } => Object::Boolean(*value),
            Expression::Prefix {
                operator, right, ..
            } => Self::eval_prefix_expression(operator, right)?,
            Expression::Infix {
                left,
                operator,
                right,
                ..
            } => Self::eval_infix_expression(left, operator, right)?,
            Expression::If {
                condition,
                consequence,
                alternative,
                ..
            } => Self::eval_conditional_expression(condition, consequence, alternative)?,
            Expression::Block { statements, .. } => Self::eval_block_expression(statements)?,
            _ => Object::Null,
        };

        Ok(evaluated)
    }

    fn eval_statement(stmt: &Statement) -> Result<Object> {
        let evaluated = match stmt {
            Statement::Expression { expression, .. } => Self::eval_expression(expression)?,
            Statement::Return { value, .. } => Self::eval_return_statement(value)?,
            _ => Object::Null,
        };

        Ok(evaluated)
    }

    fn eval_program(prog: &Program) -> Result<Object> {
        let mut result = Object::Null;

        for stmt in prog.statements().iter() {
            result = Self::eval_statement(stmt)?;

            if let Object::ReturnValue(value) = result {
                return Ok(*value);
            }
        }

        Ok(result)
    }

    fn eval_prefix_expression(operator: &str, right: &Expression) -> Result<Object> {
        let right = Self::eval_expression(right)?;

        let evaluated = match operator {
            "!" => right.bang()?,
            "-" => right.negate()?,
            _ => {
                return Err(EvalError::UnsupportedPrefixOperator {
                    right: right.object_type(),
                    operator: operator.to_string(),
                });
            }
        };

        Ok(evaluated)
    }

    fn eval_infix_expression(
        left: &Expression,
        operator: &str,
        right: &Expression,
    ) -> Result<Object> {
        let left = Self::eval_expression(left)?;
        let right = Self::eval_expression(right)?;

        let evaluated = match operator {
            "+" => left.add(right)?,
            "-" => left.subtract(right)?,
            "*" => left.multiply(right)?,
            "/" => left.divide(right)?,
            "<" => left.less_than(right)?,
            ">" => left.greater_than(right)?,
            "==" => left.equal(right)?,
            "!=" => left.not_equal(right)?,
            other => {
                return Err(EvalError::UnsupportedInfixOperator {
                    left: left.object_type(),
                    right: right.object_type(),
                    operator: other.to_string(),
                });
            }
        };

        Ok(evaluated)
    }

    fn eval_conditional_expression(
        condition: &Expression,
        consequence: &Expression,
        alternative: &Option<Expression>,
    ) -> Result<Object> {
        let condition = Self::eval_expression(condition)?;

        if condition.is_truthy() {
            Self::eval_expression(consequence)
        } else if let Some(alternative) = alternative {
            Self::eval_expression(alternative)
        } else {
            Ok(Object::Null)
        }
    }

    fn eval_block_expression(statements: &Vec<Statement>) -> Result<Object> {
        let mut result = Object::Null;

        for stmt in statements.iter() {
            result = Self::eval_statement(stmt)?;

            if result.is_return_value() {
                return Ok(result);
            }
        }

        Ok(result)
    }

    fn eval_return_statement(value: &Expression) -> Result<Object> {
        let value = Self::eval_expression(value)?;

        Ok(Object::return_value(value))
    }
}

type Result<T> = std::result::Result<T, EvalError>;

#[cfg(test)]
mod tests {
    use super::Evaluator;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
        }
    }

    #[test]
    fn test_if_expressions() {
        let tests = vec![
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            match expected {
                Object::Integer(expected) => test_integer_object(evaluated, expected),
                Object::Null => assert!(evaluated.is_null()),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 10;", Object::Integer(10)),
            ("return 10; 9;", Object::Integer(10)),
            ("return 2 * 5; 9;", Object::Integer(10)),
            ("9; return 2 * 5; 9;", Object::Integer(10)),
            (
                r#"
                    if (10 > 1) {
                        if (10 > 1) {
                            return 10;
                        }

                        return 1;
                    }
                "#,
                Object::Integer(10),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            match expected {
                Object::Integer(expected) => test_integer_object(evaluated, expected),
                _ => unreachable!(),
            }
        }
    }

    fn test_integer_object(object: Object, expected: i64) {
        let Object::Integer(value) = object else {
            panic!("object is not an integer, got {object:?}");
        };

        assert_eq!(value, expected);
    }

    fn test_boolean_object(object: Object, expected: bool) {
        let Object::Boolean(value) = object else {
            panic!("object is not a boolean, got {object:?}");
        };

        assert_eq!(value, expected);
    }

    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if !parser.errors().is_empty() {
            for error in parser.errors() {
                eprintln!("parser error: {error}");
            }

            panic!("parser has errors");
        }

        Evaluator::eval(&program.into()).expect("evaluation failed")
    }
}
