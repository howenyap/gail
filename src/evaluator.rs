use crate::ast::Node;
use crate::ast::{Expression, Program, Statement};
use crate::object::Object;

pub struct Evaluator;

impl Evaluator {
    pub fn eval(node: &Node) -> Object {
        match node {
            Node::Expression(expr) => Self::eval_expression(expr),
            Node::Statement(stmt) => Self::eval_statement(stmt),
            Node::Program(prog) => Self::eval_program(prog),
        }
    }

    fn eval_expression(expr: &Expression) -> Object {
        match expr {
            Expression::Int { value, .. } => Object::Integer(*value),
            Expression::Bool { value, .. } => Object::Boolean(*value),
            _ => Object::Null,
        }
    }

    fn eval_statement(stmt: &Statement) -> Object {
        match stmt {
            Statement::Expression { expression, .. } => Self::eval_expression(expression),
            _ => Object::Null,
        }
    }

    fn eval_program(prog: &Program) -> Object {
        let mut result = Object::Null;

        for stmt in prog.statements().iter() {
            result = Self::eval_statement(stmt);
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::Evaluator;
    use crate::lexer::Lexer;
    use crate::object::Object;
    use crate::parser::Parser;

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![("5", 5), ("10", 10)];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![("true", true), ("false", false)];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            test_boolean_object(evaluated, expected);
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
        assert!(parser.errors().is_empty());

        Evaluator::eval(&program.into())
    }
}
