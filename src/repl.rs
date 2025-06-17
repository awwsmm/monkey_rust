use crate::object::ObjectLike;
use crate::{ast, compiler, lexer, object, parser, vm};
use std::io::{BufRead, Write};

const PROMPT: &'static [u8] = ">> ".as_bytes();

pub(crate) fn start(reader: &mut impl BufRead, writer: &mut impl Write) {

    let mut constants = vec![];
    let mut globals = [const { None }; vm::GLOBALS_SIZE];
    let mut symbol_table = compiler::symbol_table::SymbolTable::new();
    for (i, v) in object::builtins::BUILTINS.iter().enumerate() {
        symbol_table.define_builtin(i, v.name);
    }

    loop {
        writer.write(PROMPT).unwrap();
        writer.flush().unwrap();

        let mut line = String::new();
        reader.read_line(&mut line).unwrap();

        if line.len() < 1 {
            return;
        }

        let l = lexer::Lexer::new(line);
        let mut p = parser::Parser::new(l);

        let program = p.parse_program();
        if p.errors.len() != 0 {
            print_parser_errors(writer, p.errors);
            continue
        }

        let mut comp = compiler::Compiler::new_with_state(symbol_table.clone(), constants.clone());
        let err = comp.compile(ast::Node::Program(program));
        if let Some(err) = err {
            write!(writer, "Woops! Compilation failed:\n {}\n", err).unwrap();
            continue
        }

        let code = comp.bytecode().clone();
        constants = code.constants.clone();

        let mut machine = vm::VM::new_with_globals_store(code, globals.clone());
        let err = machine.run();
        if let Some(err) = err {
            write!(writer, "Woops! Executing bytecode failed:\n {}\n", err).unwrap();
            continue
        }

        let last_popped = machine.last_popped_stack_elem();
        write!(writer, "{}\n", last_popped.unwrap().inspect()).unwrap();

        symbol_table = comp.symbol_table;
        globals = machine.globals;
    }
}

const MONKEY_FACE: &'static str = r#"            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._    _./ |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

fn print_parser_errors(writer: &mut impl Write, errors: Vec<String>) {
    write!(writer, "{}", MONKEY_FACE).unwrap();
    write!(writer, "{}", "Woops! We ran into some monkey business here!\n").unwrap();
    write!(writer, "{}", " parser errors:\n").unwrap();
    for msg in errors.iter() {
        write!(writer, "\t{}\n", msg).unwrap();
    }
}