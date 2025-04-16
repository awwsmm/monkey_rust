use crate::{ast, evaluator, lexer, parser};
use std::io::{BufRead, Write};

const PROMPT: &'static [u8] = ">> ".as_bytes();

pub(crate) fn start(reader: &mut impl BufRead, writer: &mut impl Write) {
    loop {
        writer.write(PROMPT).unwrap();
        writer.flush().unwrap();

        let mut line = String::new();
        reader.read_line(&mut line).unwrap();

        if line.len() < 1 {
            return;
        }

        let l = lexer::Lexer::new(line.as_str());
        let mut p = parser::Parser::new(l);

        let program = p.parse_program();
        if p.errors.len() != 0 {
            print_parser_errors(writer, p.errors);
            continue
        }

        let evaluated = evaluator::eval(Some(ast::Node::Program(program)));
        if let Some(evaluated) = evaluated {
            write!(writer, "{}\n", evaluated.inner().inspect()).unwrap();
        }
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