use crate::lexer::Lexer;
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
            for token in lexer {
                writeln!(output, "{token:?}")?;
            }
        }

        Ok(())
    }
}
