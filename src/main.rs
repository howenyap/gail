use gail::Repl;

fn main() {
    if let Err(e) = Repl::run() {
        eprintln!("Error: {e}");
    }
}
