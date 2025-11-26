use crate::ast::{Expression, Program, Statement};
use crate::environment::Env;
use crate::error::EvalError;
use crate::object::{FunctionType, Object, ObjectTrait, ObjectType};

pub struct Evaluator;

impl Evaluator {
    pub fn new() -> Self {
        Self
    }

    pub fn eval(&self, program: &Program, env: Env) -> Result<Object> {
        self.eval_program(program, env)
    }

    fn eval_expression(&self, expr: &Expression, env: Env) -> Result<Object> {
        let evaluated = match expr {
            Expression::Int { value } => Object::Integer(*value),
            Expression::Bool { value } => Object::Boolean(*value),
            Expression::String { value } => Object::String(value.clone()),
            Expression::Prefix { operator, right } => {
                self.eval_prefix_expression(operator, right, env)?
            }
            Expression::Infix {
                left,
                operator,
                right,
            } => self.eval_infix_expression(left, operator, right, env)?,
            Expression::If {
                condition,
                consequence,
                alternative,
            } => self.eval_conditional_expression(condition, consequence, alternative, env)?,
            Expression::Block { statements } => self.eval_block_expression(statements, env)?,
            Expression::Ident { value } => self.eval_identifier(value, env.clone())?,
            Expression::Function { parameters, body } => {
                self.eval_function_expression(parameters, body, env)
            }
            Expression::Call {
                function,
                arguments,
            } => self.eval_call_expression(function, arguments, env)?,
        };

        Ok(evaluated)
    }

    fn eval_statement(&self, stmt: &Statement, env: Env) -> Result<Object> {
        match stmt {
            Statement::Expression { expression } => self.eval_expression(expression, env),
            Statement::Return { value } => self.eval_return_statement(value, env),
            Statement::Let { name, value } => self.eval_let_statement(name, value, env),
        }
    }

    fn eval_program(&self, prog: &Program, env: Env) -> Result<Object> {
        let mut result = Object::Null;

        for stmt in prog.statements().iter() {
            result = self.eval_statement(stmt, env.clone())?;

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
        env: Env,
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
        env: Env,
    ) -> Result<Object> {
        let left = self.eval_expression(left, env.clone())?;
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
        env: Env,
    ) -> Result<Object> {
        let condition = self.eval_expression(condition, env.clone())?;

        if condition.is_truthy() {
            self.eval_expression(consequence, env)
        } else if let Some(alternative) = alternative {
            self.eval_expression(alternative, env)
        } else {
            Ok(Object::Null)
        }
    }

    fn eval_block_expression(&self, statements: &[Statement], env: Env) -> Result<Object> {
        let mut result = Object::Null;

        for stmt in statements.iter() {
            result = self.eval_statement(stmt, env.clone())?;

            if result.is_return_value() {
                return Ok(result);
            }
        }

        Ok(result)
    }

    fn eval_function_expression(
        &self,
        parameters: &[Expression],
        body: &Expression,
        env: Env,
    ) -> Object {
        let parameters: Vec<String> = parameters.iter().map(|p| p.to_string()).collect();

        Object::Function {
            parameters,
            body: Box::new(body.clone()),
            env: env.clone(),
        }
    }

    fn eval_call_expression(
        &self,
        function: &Expression,
        arguments: &[Expression],
        env: Env,
    ) -> Result<Object> {
        let function = self.eval_expression(function, env.clone())?;

        if let Object::BuiltinFunction(function_type) = function {
            return self.eval_builtin_function(&function_type, arguments, env.clone());
        }

        let Object::Function {
            parameters,
            body,
            env: outer_env,
        } = function
        else {
            return Err(EvalError::NotAFunction {
                function: function.object_type(),
            });
        };

        let arguments = arguments
            .iter()
            .map(|a| self.eval_expression(a, env.clone()))
            .collect::<Result<Vec<Object>>>()?;

        if parameters.len() != arguments.len() {
            return Err(EvalError::InvalidArgumentCount {
                expected: parameters.len(),
                got: arguments.len(),
            });
        }

        let bindings = parameters.into_iter().zip(arguments).collect();
        let inner_env = outer_env.extend(bindings);
        let result = self.eval_expression(&body, inner_env)?;

        Ok(match result {
            Object::ReturnValue(value) => *value,
            other => other,
        })
    }

    fn eval_identifier(&self, name: &str, env: Env) -> Result<Object> {
        if let Some(value) = env.get(name) {
            return Ok(value);
        }

        match name {
            "len" => Ok(Object::BuiltinFunction(FunctionType::Len)),
            _ => Err(EvalError::IdentifierNotFound {
                name: name.to_string(),
            }),
        }
    }

    fn eval_builtin_function(
        &self,
        function_type: &FunctionType,
        arguments: &[Expression],
        env: Env,
    ) -> Result<Object> {
        match function_type {
            FunctionType::Len => {
                let [argument] = arguments else {
                    return Err(EvalError::InvalidArgumentCount {
                        expected: 1,
                        got: arguments.len(),
                    });
                };

                let argument = self.eval_expression(argument, env)?;

                let Object::String(value) = argument else {
                    return Err(EvalError::InvalidArgumentType {
                        expected: ObjectType::String,
                        got: argument.object_type(),
                    });
                };

                let length = value.len() as i64;

                Ok(Object::Integer(length))
            }
        }
    }

    fn eval_return_statement(&self, value: &Expression, env: Env) -> Result<Object> {
        let value = self.eval_expression(value, env)?;

        Ok(Object::return_value(value))
    }

    fn eval_let_statement(
        &self,
        name: &Expression,
        value: &Expression,
        env: Env,
    ) -> Result<Object> {
        let value = self.eval_expression(value, env.clone())?;

        let Expression::Ident { value: name, .. } = name else {
            return Err(EvalError::IdentifierNotFound {
                name: name.to_string(),
            });
        };

        env.set(name, value);

        Ok(Object::Null)
    }
}

type Result<T> = std::result::Result<T, EvalError>;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Program;
    use crate::environment::Env;
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
    fn test_eval_string_expression() {
        let tests = vec![
            ("\"rust\"", "rust"),
            (
                "\"writing \" + \"an \" + \"interpreter\"",
                "writing an interpreter",
            ),
            ("3 * \"hi\"", "hihihi"),
        ];

        for (input, expected) in tests.iter() {
            let evaluated = test_eval(input);
            test_string_object(evaluated, expected);
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
    fn test_function_object() {
        let input = "fn(x) { x + 1; }";
        let evaluated = test_eval(input);

        let Object::Function {
            parameters, body, ..
        } = evaluated
        else {
            panic!("object is not a function, got {evaluated:?}");
        };

        let expected_parameters = vec!["x".to_string()];
        assert_eq!(parameters, expected_parameters);

        let expected_body = "(x + 1)";
        assert_eq!(body.to_string(), expected_body);
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            (
                "let identity = fn(x) { x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let double = fn(x) { x * 2; }; double(5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5, 5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Object::Integer(20),
            ),
            ("fn(x) { x; }(5);", Object::Integer(5)),
            (
                r#"
                    let factorial = fn(n) {
                        if (n == 0) {
                            return 1;
                        } else {
                            return n * factorial(n - 1);
                        }
                    };
                    factorial(5);
                "#,
                Object::Integer(120),
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
            ("-true", "unsupported prefix operator: -Boolean"),
            (
                "true + false;",
                "unsupported infix operator: Boolean + Boolean",
            ),
            (
                "5; true + false; 5",
                "unsupported infix operator: Boolean + Boolean",
            ),
            ("foobar", "identifier not found: foobar"),
            (
                r#"
                    let add = fn(x, y) { x + y };
                    add(1);
                "#,
                "expected 2 arguments, got 1 instead",
            ),
            ("let a = 1; a();", "not a function: Integer"),
            ("1 / 0;", "division by zero"),
            ("len(1)", "expected String, got Integer instead"),
            (
                "len(\"hello\", \"world\")",
                "expected 1 argument, got 2 instead",
            ),
        ];

        for (input, expected) in tests {
            test_error_object(input, expected);
        }
    }

    #[test]
    fn test_closure() {
        let input = "
            let newAdder = fn(x) {
                fn(y) { x + y };
            };
            let addTwo = newAdder(2);
            addTwo(2);
        ";

        let evaluated = test_eval(input);
        test_object(evaluated, Object::Integer(4));
    }

    #[test]
    fn test_builtin_functions() {
        let tests = vec![
            ("len(\"\")", Object::Integer(0)),
            ("len(\"four\")", Object::Integer(4)),
            ("len(\"hello world\")", Object::Integer(11)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_object(evaluated, expected);
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

    fn test_string_object(object: Object, expected: &str) {
        let Object::String(value) = object else {
            panic!("object is not a string, got {object:?}");
        };

        assert_eq!(value, expected);
    }

    fn test_error_object(input: &str, expected: &str) {
        let program = Program::from_str(input).expect("program construction failed");
        let evaluator = Evaluator::new();
        let env = Env::new();

        match evaluator.eval(&program, env) {
            Ok(_) => panic!("expected error but got no error"),
            Err(error) => assert_eq!(error.to_string(), expected),
        }
    }

    fn test_eval(input: &str) -> Object {
        let program = Program::from_str(input).expect("program construction failed");
        let evaluator = Evaluator::new();
        let env = Env::new();

        evaluator.eval(&program, env).expect("evaluation failed")
    }
}
