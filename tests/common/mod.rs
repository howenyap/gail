use gail::ast::Program;
use gail::environment::Env;
use gail::evaluator::Evaluator;
use gail::object::ObjectTrait;

pub fn run(input: &str) -> String {
    let program = Program::from_str(input).expect("program construction failed");
    let evaluator = Evaluator::new();
    let env = Env::new();

    evaluator
        .eval(&program, env)
        .expect("evaluation failed")
        .inspect()
}
