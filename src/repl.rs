use crate::{lexer, token};
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

        let mut l = lexer::Lexer::new(line.as_str());

        loop {
            let tok = l.next_token();

            if tok.token_type == token::TokenType::EOF {
                break;
            }

            write!(writer, "{}\n", tok).unwrap();
            writer.flush().unwrap();
        }
    }
}