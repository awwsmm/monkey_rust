use std::io;
use std::io::Write;

mod token;
mod lexer;
mod repl;
mod ast;

fn main() {
    let mut stdout = io::stdout();

    stdout.write("Hello! This is the Monkey programming language!\n".as_bytes()).unwrap();
    stdout.write("Feel free to type in commands\n".as_bytes()).unwrap();

    repl::start(&mut io::stdin().lock(), &mut stdout)
}
