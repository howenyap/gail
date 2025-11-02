mod ast;
mod lexer;
mod parser;
mod repl;
mod token;
use repl::Repl;

fn main() {
    if let Err(e) = Repl::run() {
        eprintln!("Error: {e}");
    }
}
