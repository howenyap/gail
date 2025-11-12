use crate::ast::Node;
use crate::ast::{Expression, Program, Statement};
use crate::environment::Environment;
use crate::error::EvalError;
use crate::object::{Object, ObjectTrait};

pub struct Evaluator;

impl Evaluator {
    pub fn new() -> Self {
        Self
    }

    pub fn eval(&self, node: &Node, env: &mut Environment) -> Result<Object> {
        match node {
            Node::Expression(expr) => self.eval_expression(&expr, env),
            Node::Statement(stmt) => self.eval_statement(&stmt, env),
            Node::Program(prog) => self.eval_program(&prog, env),
        }
    }

    fn eval_expression(&self, expr: &Expression, env: &mut Environment) -> Result<Object> {
        let evaluated = match expr {
            Expression::Int { value, .. } => Object::Integer(*value),
            Expression::Bool { value, .. } => Object::Boolean(*value),
            Expression::Prefix {
                operator, right, ..
            } => self.eval_prefix_expression(operator, right, env)?,
            Expression::Infix {
                left,
                operator,
                right,
                ..
            } => self.eval_infix_expression(left, operator, right, env)?,
            Expression::If {
                condition,
                consequence,
                alternative,
                ..
            } => self.eval_conditional_expression(condition, consequence, alternative, env)?,
            Expression::Block { statements, .. } => self.eval_block_expression(statements, env)?,
            Expression::Ident { value, .. } => self.eval_identifier(value, &*env)?,
            _ => todo!(),
        };

        Ok(evaluated)
    }

    fn eval_statement(&self, stmt: &Statement, env: &mut Environment) -> Result<Object> {
        match stmt {
            Statement::Expression { expression, .. } => self.eval_expression(expression, env),
            Statement::Return { value, .. } => self.eval_return_statement(value, env),
            Statement::Let { name, value, .. } => self.eval_let_statement(name, value, env),
        }
    }

    fn eval_program(&self, prog: &Program, env: &mut Environment) -> Result<Object> {
        let mut result = Object::Null;

        for stmt in prog.statements().iter() {
            result = self.eval_statement(stmt, env)?;

            if let Object::ReturnValue(value) = result {
                return Ok(*value);
            }
        }

        Ok(result)
    }

    fn eval_prefix_expression(
        &self,
        operator: &str,
        right: &Expression,
        env: &mut Environment,
    ) -> Result<Object> {
        let right = self.eval_expression(right, env)?;

        match operator {
            "!" => right.bang(),
            "-" => right.negate(),
            _ => Err(EvalError::UnsupportedPrefixOperator {
                right: right.object_type(),
                operator: operator.to_string(),
            }),
        }
    }

    fn eval_infix_expression(
        &self,
        left: &Expression,
        operator: &str,
        right: &Expression,
        env: &mut Environment,
    ) -> Result<Object> {
        let left = self.eval_expression(left, env)?;
        let right = self.eval_expression(right, env)?;

        match operator {
            "+" => left.add(right),
            "-" => left.subtract(right),
            "*" => left.multiply(right),
            "/" => left.divide(right),
            "<" => left.less_than(right),
            ">" => left.greater_than(right),
            "==" => left.equal(right),
            "!=" => left.not_equal(right),
            other => Err(EvalError::UnsupportedInfixOperator {
                left: left.object_type(),
                right: right.object_type(),
                operator: other.to_string(),
            }),
        }
    }

    fn eval_conditional_expression(
        &self,
        condition: &Expression,
        consequence: &Expression,
        alternative: &Option<Expression>,
        env: &mut Environment,
    ) -> Result<Object> {
        let condition = self.eval_expression(condition, env)?;

        if condition.is_truthy() {
            self.eval_expression(consequence, env)
        } else if let Some(alternative) = alternative {
            self.eval_expression(alternative, env)
        } else {
            Ok(Object::Null)
        }
    }

    fn eval_block_expression(
        &self,
        statements: &Vec<Statement>,
        env: &mut Environment,
    ) -> Result<Object> {
        let mut result = Object::Null;

        for stmt in statements.iter() {
            result = self.eval_statement(stmt, env)?;

            if result.is_return_value() {
                return Ok(result);
            }
        }

        Ok(result)
    }

    fn eval_identifier(&self, name: &str, env: &Environment) -> Result<Object> {
        env.get(name)
            .cloned()
            .ok_or_else(|| EvalError::IdentifierNotFound {
                name: name.to_string(),
            })
    }

    fn eval_return_statement(&self, value: &Expression, env: &mut Environment) -> Result<Object> {
        let value = self.eval_expression(value, env)?;

        Ok(Object::return_value(value))
    }

    fn eval_let_statement(
        &self,
        name: &Expression,
        value: &Expression,
        env: &mut Environment,
    ) -> Result<Object> {
        let value = self.eval_expression(value, env)?;
        let name_str = match name {
            Expression::Ident { value, .. } => value.to_string(),
            _ => {
                return Err(EvalError::IdentifierNotFound {
                    name: name.to_string(),
                });
            }
        };
        env.set(name_str, value);

        Ok(Object::Null)
    }
}

type Result<T> = std::result::Result<T, EvalError>;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Program;
    use crate::environment::Environment;
    use crate::object::Object;

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
            test_object(evaluated, expected);
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
            test_object(evaluated, expected);
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = vec![
            ("let a = 5; a;", Object::Integer(5)),
            ("let a = 5 * 5; a;", Object::Integer(25)),
            ("let a = 5; let b = a; b;", Object::Integer(5)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Object::Integer(15),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_object(evaluated, expected);
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "unsupported infix operator: Integer + Boolean"),
            (
                "5 + true; 5;",
                "unsupported infix operator: Integer + Boolean",
            ),
            ("-true", "expected type Integer, got Boolean instead"),
            (
                "true + false;",
                "unsupported infix operator: Boolean + Boolean",
            ),
            (
                "5; true + false; 5",
                "unsupported infix operator: Boolean + Boolean",
            ),
            ("foobar", "identifier not found: foobar"),
        ];

        for (input, expected) in tests {
            test_error_object(input, expected);
        }
    }

    fn test_object(object: Object, expected: Object) {
        match (object, expected) {
            (Object::Integer(left), Object::Integer(right)) => assert_eq!(left, right),
            (Object::Boolean(left), Object::Boolean(right)) => assert_eq!(left, right),
            (Object::Null, Object::Null) => (),
            (Object::ReturnValue(left), Object::ReturnValue(right)) => test_object(*left, *right),
            _ => unreachable!(),
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

    fn test_error_object(input: &str, expected: &str) {
        let program = Program::from_str(input).expect("program construction failed");
        let evaluator = Evaluator::new();
        let mut env = Environment::new();

        match evaluator.eval(&Node::Program(program), &mut env) {
            Ok(_) => panic!("expected error but got no error"),
            Err(error) => assert_eq!(error.to_string(), expected),
        }
    }

    fn test_eval(input: &str) -> Object {
        let program = Program::from_str(input).expect("program construction failed");
        let evaluator = Evaluator::new();
        let mut env = Environment::new();

        evaluator
            .eval(&program.into(), &mut env)
            .expect("evaluation failed")
    }
}
