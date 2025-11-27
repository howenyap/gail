use crate::ast::Program;
use crate::environment::Env;
use crate::evaluator::Evaluator;
use std::io;
use std::io::Write;

pub struct Repl;

const INTRO: &str = "Welcome to Gail! Type \\exit to exit.";
const PROMPT: &str = "$ ";
const EXIT_COMMAND: &str = "\\exit";

impl Repl {
    pub fn run() -> io::Result<()> {
        let input = io::stdin();
        let mut output = io::stdout();
        let mut line = String::new();

        let evaluator = Evaluator::new();
        let env = Env::new();

        writeln!(output, "{INTRO}")?;

        loop {
            write!(output, "{PROMPT}")?;
            output.flush()?;

            input.read_line(&mut line)?;
            if line.is_empty() || line.trim() == EXIT_COMMAND {
                break;
            }

            let Ok(program) = Program::from_input(&line) else {
                line.clear();
                continue;
            };

            match evaluator.eval(&program, env.clone()) {
                Ok(evaluated) => writeln!(output, "{evaluated}")?,
                Err(error) => writeln!(output, "ERROR: {error}")?,
            }

            line.clear();
        }

        Ok(())
    }
}
