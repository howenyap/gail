use crate::ast::Program;
use crate::environment::Env;
use crate::evaluator::Evaluator;
use std::io::{self, Write};

pub struct Repl {
    accumulated_input: String,
    evaluator: Evaluator,
    env: Env,
}

const INTRO: &str = "Welcome to Gail! Type \\exit to exit.";
const NEWLINE_PROMPT: &str = "$ ";
const CONTINUATION_PROMPT: &str = "  ";
const EXIT_COMMAND: &str = "\\exit";

impl Repl {
    pub fn run() -> io::Result<()> {
        let mut repl = Self::new();
        repl.start()
    }

    fn new() -> Self {
        Self {
            accumulated_input: String::new(),
            evaluator: Evaluator::new(),
            env: Env::new(),
        }
    }

    fn start(&mut self) -> io::Result<()> {
        let mut output = io::stdout();

        writeln!(output, "{INTRO}")?;

        loop {
            let prompt = if self.has_pending_input() {
                CONTINUATION_PROMPT
            } else {
                NEWLINE_PROMPT
            };

            write!(output, "{prompt}")?;
            output.flush()?;

            let mut input = String::new();
            io::stdin().read_line(&mut input)?;
            let input = input.trim();

            if input == EXIT_COMMAND {
                break;
            }

            if input.is_empty() {
                continue;
            }

            self.append_input(input);

            if !self.is_input_complete() {
                continue;
            }

            let program = match Program::from_input(&self.accumulated_input) {
                Ok(program) => program,
                Err(errors) => {
                    self.accumulated_input.clear();

                    for error in errors {
                        writeln!(output, "ERROR: {error}")?;
                    }

                    continue;
                }
            };

            match self.evaluator.eval(&program, self.env.clone()) {
                Ok(evaluated) => writeln!(output, "{evaluated}")?,
                Err(error) => writeln!(output, "ERROR: {error}")?,
            }

            self.accumulated_input.clear();
        }

        Ok(())
    }

    fn append_input(&mut self, input: &str) {
        self.accumulated_input.push_str(input);
    }

    fn has_pending_input(&self) -> bool {
        !self.accumulated_input.is_empty()
    }

    fn is_input_complete(&self) -> bool {
        let mut stack = Vec::new();
        let mut in_string = false;

        for ch in self.accumulated_input.chars() {
            match ch {
                '"' => {
                    in_string = !in_string;
                    continue;
                }
                _ if in_string => continue,
                '(' | '{' | '[' => stack.push(ch),
                ')' => {
                    if stack.pop() != Some('(') {
                        return false;
                    }
                }
                '}' => {
                    if stack.pop() != Some('{') {
                        return false;
                    }
                }
                ']' => {
                    if stack.pop() != Some('[') {
                        return false;
                    }
                }
                _ => {}
            }
        }

        stack.is_empty() && !in_string
    }
}

impl Default for Repl {
    fn default() -> Self {
        Self::new()
    }
}
