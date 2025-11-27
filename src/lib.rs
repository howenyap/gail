pub mod ast;
pub mod environment;
pub mod error;
pub mod evaluator;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod repl;
pub mod token;
pub mod utils;

pub use repl::Repl;

use crate::ast::Program;
use crate::environment::Env;
use crate::error::InterpreterError;
use crate::evaluator::Evaluator;

pub fn run(input: &str) -> Result<(), InterpreterError> {
    let program = Program::from_input(input)?;
    let evaluator = Evaluator::new();
    let env = Env::new();

    evaluator.eval(&program, env)?;

    Ok(())
}
