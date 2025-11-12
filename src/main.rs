mod ast;
mod environment;
mod error;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

use repl::Repl;

fn main() {
    if let Err(e) = Repl::run() {
        eprintln!("Error: {e}");
    }
}
