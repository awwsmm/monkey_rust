use crate::{lexer, parser};
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

        write!(writer, "{}\n", program.to_string());
    }
}

fn print_parser_errors(writer: &mut impl Write, errors: Vec<String>) {
    for msg in errors.iter() {
        write!(writer, "\t{}\n", msg).unwrap();
    }
}