use std::cell::RefCell;
use std::env;
use std::rc::Rc;
use std::time::SystemTime;

mod token;
mod lexer;
mod repl;
mod ast;
mod parser;
mod parser_tracing;
mod object;
mod evaluator;
mod code;
mod compiler;
mod vm;

const INPUT: &str = r#"
    let fibonacci = fn(x) {
        if (x == 0) {
            return 0;
        } else {
            if (x == 1) {
                return 1;
            } else {
                fibonacci(x - 1) + fibonacci(x - 2);
            }
        }
    };
    fibonacci(35);
    "#;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("exactly one argument required")
    }

    let engine = args[1].clone();

    if engine != "vm" && engine != "eval" {
        panic!("argument must be 'vm' or 'eval'")
    }

    let l = lexer::Lexer::new(INPUT);
    let mut p = parser::Parser::new(l);
    let program = p.parse_program();

    let (result, duration) = if engine == "vm" {
        let mut comp = compiler::Compiler::new();
        let err = comp.compile(ast::Node::Program(program));
        if let Some(err) = err {
            panic!("compiler error: {}", err.message)
        }

        let mut machine = vm::VM::new(comp.bytecode());

        let start = SystemTime::now();

        let err = machine.run();
        if let Some(err) = err {
            panic!("vm error: {}", err.message)
        }

        let duration = start.elapsed().unwrap();
        let result = machine.last_popped_stack_elem().unwrap().clone();

        (result, duration)
    } else {
        let env = Rc::new(RefCell::new(object::environment::Environment::new(None)));
        let start = SystemTime::now();
        let result = evaluator::eval(Some(ast::Node::Program(program)), env).unwrap();
        let duration = start.elapsed().unwrap();

        (result, duration)
    };

    println!("engine={}, result={:?}, duration={:?}", engine, result, duration)
}