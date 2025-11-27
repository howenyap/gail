use std::path::PathBuf;

use clap::{Parser, Subcommand};
use gail::{Repl, run};
use thiserror::Error;

#[derive(Parser, Debug)]
#[command(name = "gail")]
struct Args {
    #[command(subcommand)]
    command: Option<Command>,

    file: Option<PathBuf>,
}

#[derive(Subcommand, Debug)]
enum Command {
    Repl,
}

#[derive(Error, Debug)]
enum AppError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Interpreter error: {0}")]
    Interpret(#[from] gail::error::InterpreterError),

    #[error("No input file provided")]
    MissingInputFile,
}

fn main() {
    let args = Args::parse();

    if let Err(e) = run_app(args) {
        eprintln!("{e}");
    }
}

fn run_app(args: Args) -> Result<(), AppError> {
    match args.command {
        Some(Command::Repl) => Repl::run()?,
        None => {
            let file_path = args.file.ok_or(AppError::MissingInputFile)?;
            let input = std::fs::read_to_string(&file_path)?;

            run(&input)?;
        }
    }

    Ok(())
}
