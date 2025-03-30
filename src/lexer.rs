use crate::token;

#[derive(Default)]
struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    fn new(input: String) -> Self {
        Self {
            input,
            ..Default::default()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let input = String::from("=+(){},;");

        struct Test {
            expected_type: token::TokenType,
            expected_literal: String,
        }

        impl Test {
            fn new(expected_type: token::TokenType, expected_literal: &str) -> Self {
                Self { expected_type, expected_literal: String::from(expected_literal) }
            }
        }

        let tests = [
            Test::new(token::TokenType::ASSIGN, "="),
            Test::new(token::TokenType::PLUS, "+"),
            Test::new(token::TokenType::LPAREN, "("),
            Test::new(token::TokenType::RPAREN, ")"),
            Test::new(token::TokenType::LBRACE, "{"),
            Test::new(token::TokenType::RBRACE, "}"),
            Test::new(token::TokenType::COMMA, ","),
            Test::new(token::TokenType::SEMICOLON, ";"),
            Test::new(token::TokenType::EOF, ""),
        ];

        let mut l = Lexer::new(input);

        for (i, tt) in tests.iter().enumerate() {
            let tok = l.next_token();

            if tok.token_type != tt.expected_type {
                panic!("tests[{}] - tokentype wrong. expected={}, got={}",
                       i, tt.expected_type, tok.token_type)
            }

            if tok.literal != tt.expected_literal {
                panic!("tests[{}] - literal wrong. expected={}, got={}",
                       i, tt.expected_literal, tok.literal)
            }
        }
    }
}