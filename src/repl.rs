use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io;
use std::io::Write;

pub struct Repl;

impl Repl {
    pub fn run() -> io::Result<()> {
        let input = io::stdin();
        let mut output = io::stdout();
        let mut line = String::new();

        loop {
            write!(output, ">> ")?;
            output.flush()?;

            input.read_line(&mut line)?;
            if line.is_empty() {
                break;
            }

            let lexer = Lexer::new(&line);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            if !parser.errors().is_empty() {
                for error in parser.errors() {
                    writeln!(output, "Parser error: {error}")?;
                }
            } else {
                let evaluated = Evaluator::eval(&program.into());
                writeln!(output, "{evaluated}")?;
            }

            line.clear();
        }

        Ok(())
    }
}
