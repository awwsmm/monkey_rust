use crate::token;

#[derive(Default)]
pub(crate) struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    pub(crate) fn new(input: &str) -> Self {
        let mut l = Self {
            input: String::from(input),
            ..Default::default()
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0
        } else {
            self.ch = self.input.as_bytes()[self.read_position]
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub(crate) fn next_token(&mut self) -> token::Token {
        let mut tok = token::Token::default();

        self.skip_whitespace();

        match self.ch {
            0 => tok = token::Token::new(token::TokenType::EOF, String::from("")),

            b'=' => {
                if self.peek_char() == b'=' {
                    let ch = self.ch;
                    self.read_char();
                    let literal = String::from_utf8(vec![ch, self.ch]).unwrap();
                    tok = token::Token::new(token::TokenType::EQ, literal)
                } else {
                    tok = token::Token::new(token::TokenType::ASSIGN, String::from_utf8(vec![self.ch]).unwrap())
                }
            }
            b'+' => tok = token::Token::new(token::TokenType::PLUS, String::from_utf8(vec![self.ch]).unwrap()),
            b'-' => tok = token::Token::new(token::TokenType::MINUS, String::from_utf8(vec![self.ch]).unwrap()),
            b'!' => {
                if self.peek_char() == b'=' {
                    let ch = self.ch;
                    self.read_char();
                    let literal = String::from_utf8(vec![ch, self.ch]).unwrap();
                    tok = token::Token::new(token::TokenType::NEQ, literal)
                } else {
                    tok = token::Token::new(token::TokenType::BANG, String::from_utf8(vec![self.ch]).unwrap())
                }
            }
            b'/' => tok = token::Token::new(token::TokenType::SLASH, String::from_utf8(vec![self.ch]).unwrap()),
            b'*' => tok = token::Token::new(token::TokenType::ASTERISK, String::from_utf8(vec![self.ch]).unwrap()),

            b'<' => tok = token::Token::new(token::TokenType::LT, String::from_utf8(vec![self.ch]).unwrap()),
            b'>' => tok = token::Token::new(token::TokenType::GT, String::from_utf8(vec![self.ch]).unwrap()),

            b',' => tok = token::Token::new(token::TokenType::COMMA, String::from_utf8(vec![self.ch]).unwrap()),
            b';' => tok = token::Token::new(token::TokenType::SEMICOLON, String::from_utf8(vec![self.ch]).unwrap()),

            b'(' => tok = token::Token::new(token::TokenType::LPAREN, String::from_utf8(vec![self.ch]).unwrap()),
            b')' => tok = token::Token::new(token::TokenType::RPAREN, String::from_utf8(vec![self.ch]).unwrap()),
            b'{' => tok = token::Token::new(token::TokenType::LBRACE, String::from_utf8(vec![self.ch]).unwrap()),
            b'}' => tok = token::Token::new(token::TokenType::RBRACE, String::from_utf8(vec![self.ch]).unwrap()),

            _ => {
                if Self::is_letter(self.ch) {
                    tok.literal = self.read_identifier();
                    tok.token_type = token::TokenType::lookup_ident(tok.literal.as_str());
                    return tok;
                } else if Self::is_digit(self.ch) {
                    tok.literal = self.read_number();
                    tok.token_type = token::TokenType::INT;
                    return tok;
                } else {
                    tok = token::Token::new(token::TokenType::ILLEGAL, String::from_utf8(vec![self.ch]).unwrap())
                }
            }
        };

        self.read_char();
        tok
    }

    fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.read_position]
        }
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while Self::is_digit(self.ch) {
            self.read_char()
        }
        String::from(&self.input[position..self.position])
    }

    fn is_digit(ch: u8) -> bool {
        b'0' <= ch && ch <= b'9'
    }

    fn skip_whitespace(&mut self) {
        while self.ch == b' ' || self.ch == b'\t' || self.ch == b'\n' || self.ch == b'\r' {
            self.read_char()
        }
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while Self::is_letter(self.ch) {
            self.read_char()
        }
        String::from(&self.input[position..self.position])
    }

    fn is_letter(ch: u8) -> bool {
        b'a' <= ch && ch <= b'z' || b'A' <= ch && ch <= b'Z' || ch == b'_'
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
	return true;
} else {
	return false;
}

10 == 10;
10 != 9;
";

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
            Test::new(token::TokenType::LET, "let"),
            Test::new(token::TokenType::IDENT, "five"),
            Test::new(token::TokenType::ASSIGN, "="),
            Test::new(token::TokenType::INT, "5"),
            Test::new(token::TokenType::SEMICOLON, ";"),
            Test::new(token::TokenType::LET, "let"),
            Test::new(token::TokenType::IDENT, "ten"),
            Test::new(token::TokenType::ASSIGN, "="),
            Test::new(token::TokenType::INT, "10"),
            Test::new(token::TokenType::SEMICOLON, ";"),
            Test::new(token::TokenType::LET, "let"),
            Test::new(token::TokenType::IDENT, "add"),
            Test::new(token::TokenType::ASSIGN, "="),
            Test::new(token::TokenType::FUNCTION, "fn"),
            Test::new(token::TokenType::LPAREN, "("),
            Test::new(token::TokenType::IDENT, "x"),
            Test::new(token::TokenType::COMMA, ","),
            Test::new(token::TokenType::IDENT, "y"),
            Test::new(token::TokenType::RPAREN, ")"),
            Test::new(token::TokenType::LBRACE, "{"),
            Test::new(token::TokenType::IDENT, "x"),
            Test::new(token::TokenType::PLUS, "+"),
            Test::new(token::TokenType::IDENT, "y"),
            Test::new(token::TokenType::SEMICOLON, ";"),
            Test::new(token::TokenType::RBRACE, "}"),
            Test::new(token::TokenType::SEMICOLON, ";"),
            Test::new(token::TokenType::LET, "let"),
            Test::new(token::TokenType::IDENT, "result"),
            Test::new(token::TokenType::ASSIGN, "="),
            Test::new(token::TokenType::IDENT, "add"),
            Test::new(token::TokenType::LPAREN, "("),
            Test::new(token::TokenType::IDENT, "five"),
            Test::new(token::TokenType::COMMA, ","),
            Test::new(token::TokenType::IDENT, "ten"),
            Test::new(token::TokenType::RPAREN, ")"),
            Test::new(token::TokenType::SEMICOLON, ";"),
            Test::new(token::TokenType::BANG, "!"),
            Test::new(token::TokenType::MINUS, "-"),
            Test::new(token::TokenType::SLASH, "/"),
            Test::new(token::TokenType::ASTERISK, "*"),
            Test::new(token::TokenType::INT, "5"),
            Test::new(token::TokenType::SEMICOLON, ";"),
            Test::new(token::TokenType::INT, "5"),
            Test::new(token::TokenType::LT, "<"),
            Test::new(token::TokenType::INT, "10"),
            Test::new(token::TokenType::GT, ">"),
            Test::new(token::TokenType::INT, "5"),
            Test::new(token::TokenType::SEMICOLON, ";"),
            Test::new(token::TokenType::IF, "if"),
            Test::new(token::TokenType::LPAREN, "("),
            Test::new(token::TokenType::INT, "5"),
            Test::new(token::TokenType::LT, "<"),
            Test::new(token::TokenType::INT, "10"),
            Test::new(token::TokenType::RPAREN, ")"),
            Test::new(token::TokenType::LBRACE, "{"),
            Test::new(token::TokenType::RETURN, "return"),
            Test::new(token::TokenType::TRUE, "true"),
            Test::new(token::TokenType::SEMICOLON, ";"),
            Test::new(token::TokenType::RBRACE, "}"),
            Test::new(token::TokenType::ELSE, "else"),
            Test::new(token::TokenType::LBRACE, "{"),
            Test::new(token::TokenType::RETURN, "return"),
            Test::new(token::TokenType::FALSE, "false"),
            Test::new(token::TokenType::SEMICOLON, ";"),
            Test::new(token::TokenType::RBRACE, "}"),
            Test::new(token::TokenType::INT, "10"),
            Test::new(token::TokenType::EQ, "=="),
            Test::new(token::TokenType::INT, "10"),
            Test::new(token::TokenType::SEMICOLON, ";"),
            Test::new(token::TokenType::INT, "10"),
            Test::new(token::TokenType::NEQ, "!="),
            Test::new(token::TokenType::INT, "9"),
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