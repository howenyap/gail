use crate::ast::Program;
use crate::environment::Environment;
use crate::evaluator::Evaluator;
use std::io;
use std::io::Write;

pub struct Repl;

impl Repl {
    pub fn run() -> io::Result<()> {
        let input = io::stdin();
        let mut output = io::stdout();
        let mut line = String::new();

        let evaluator = Evaluator::new();
        let mut env = Environment::new();

        loop {
            write!(output, ">> ")?;
            output.flush()?;

            input.read_line(&mut line)?;
            if line.is_empty() {
                break;
            }

            let Ok(program) = Program::from_str(&line) else {
                line.clear();
                continue;
            };

            match evaluator.eval(&program, &mut env) {
                Ok(evaluated) => writeln!(output, "{evaluated}")?,
                Err(error) => writeln!(output, "Error: {error}")?,
            }

            line.clear();
        }

        Ok(())
    }
}
