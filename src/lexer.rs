use crate::token::{Token, TokenType};

#[derive(Default)]
pub(crate) struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    pub(crate) fn new(input: impl Into<String>) -> Self {
        let mut l = Self {
            input: input.into(),
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

    pub(crate) fn next_token(&mut self) -> Token {
        let mut tok = Token::default();

        self.skip_whitespace();

        match self.ch {
            0 => tok = Token::new(TokenType::EOF, String::from("")),

            b'=' => {
                if self.peek_char() == b'=' {
                    let ch = self.ch;
                    self.read_char();
                    let literal = String::from_utf8(vec![ch, self.ch]).unwrap();
                    tok = Token::new(TokenType::EQ, literal)
                } else {
                    tok = Token::new(TokenType::ASSIGN, String::from_utf8(vec![self.ch]).unwrap())
                }
            }
            b'+' => tok = Token::new(TokenType::PLUS, String::from_utf8(vec![self.ch]).unwrap()),
            b'-' => tok = Token::new(TokenType::MINUS, String::from_utf8(vec![self.ch]).unwrap()),
            b'!' => {
                if self.peek_char() == b'=' {
                    let ch = self.ch;
                    self.read_char();
                    let literal = String::from_utf8(vec![ch, self.ch]).unwrap();
                    tok = Token::new(TokenType::NEQ, literal)
                } else {
                    tok = Token::new(TokenType::BANG, String::from_utf8(vec![self.ch]).unwrap())
                }
            }
            b'/' => tok = Token::new(TokenType::SLASH, String::from_utf8(vec![self.ch]).unwrap()),
            b'*' => tok = Token::new(TokenType::ASTERISK, String::from_utf8(vec![self.ch]).unwrap()),

            b'<' => tok = Token::new(TokenType::LT, String::from_utf8(vec![self.ch]).unwrap()),
            b'>' => tok = Token::new(TokenType::GT, String::from_utf8(vec![self.ch]).unwrap()),

            b',' => tok = Token::new(TokenType::COMMA, String::from_utf8(vec![self.ch]).unwrap()),
            b';' => tok = Token::new(TokenType::SEMICOLON, String::from_utf8(vec![self.ch]).unwrap()),

            b'(' => tok = Token::new(TokenType::LPAREN, String::from_utf8(vec![self.ch]).unwrap()),
            b')' => tok = Token::new(TokenType::RPAREN, String::from_utf8(vec![self.ch]).unwrap()),
            b'{' => tok = Token::new(TokenType::LBRACE, String::from_utf8(vec![self.ch]).unwrap()),
            b'}' => tok = Token::new(TokenType::RBRACE, String::from_utf8(vec![self.ch]).unwrap()),
            b'[' => tok = Token::new(TokenType::LBRACKET, String::from_utf8(vec![self.ch]).unwrap()),
            b']' => tok = Token::new(TokenType::RBRACKET, String::from_utf8(vec![self.ch]).unwrap()),

            b'"' => tok = Token::new(TokenType::STRING, self.read_string()),

            _ => {
                if Self::is_letter(self.ch) {
                    tok.literal = self.read_identifier();
                    tok.token_type = TokenType::lookup_ident(tok.literal.as_str());
                    return tok;
                } else if Self::is_digit(self.ch) {
                    tok.literal = self.read_number();
                    tok.token_type = TokenType::INT;
                    return tok;
                } else {
                    tok = Token::new(TokenType::ILLEGAL, String::from_utf8(vec![self.ch]).unwrap())
                }
            }
        };

        self.read_char();
        tok
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;
        loop {
            self.read_char();
            if self.ch == b'"' || self.ch == 0 {
                break
            }
        }
        String::from(&self.input[position..self.position])
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
        let input = r#"let five = 5;
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
"foobar"
"foo bar"
[1, 2];
"#;

        struct Test {
            expected_type: TokenType,
            expected_literal: String,
        }

        impl Test {
            fn new(expected_type: TokenType, expected_literal: &str) -> Self {
                Self { expected_type, expected_literal: String::from(expected_literal) }
            }
        }

        let tests = [
            Test::new(TokenType::LET, "let"),
            Test::new(TokenType::IDENT, "five"),
            Test::new(TokenType::ASSIGN, "="),
            Test::new(TokenType::INT, "5"),
            Test::new(TokenType::SEMICOLON, ";"),
            Test::new(TokenType::LET, "let"),
            Test::new(TokenType::IDENT, "ten"),
            Test::new(TokenType::ASSIGN, "="),
            Test::new(TokenType::INT, "10"),
            Test::new(TokenType::SEMICOLON, ";"),
            Test::new(TokenType::LET, "let"),
            Test::new(TokenType::IDENT, "add"),
            Test::new(TokenType::ASSIGN, "="),
            Test::new(TokenType::FUNCTION, "fn"),
            Test::new(TokenType::LPAREN, "("),
            Test::new(TokenType::IDENT, "x"),
            Test::new(TokenType::COMMA, ","),
            Test::new(TokenType::IDENT, "y"),
            Test::new(TokenType::RPAREN, ")"),
            Test::new(TokenType::LBRACE, "{"),
            Test::new(TokenType::IDENT, "x"),
            Test::new(TokenType::PLUS, "+"),
            Test::new(TokenType::IDENT, "y"),
            Test::new(TokenType::SEMICOLON, ";"),
            Test::new(TokenType::RBRACE, "}"),
            Test::new(TokenType::SEMICOLON, ";"),
            Test::new(TokenType::LET, "let"),
            Test::new(TokenType::IDENT, "result"),
            Test::new(TokenType::ASSIGN, "="),
            Test::new(TokenType::IDENT, "add"),
            Test::new(TokenType::LPAREN, "("),
            Test::new(TokenType::IDENT, "five"),
            Test::new(TokenType::COMMA, ","),
            Test::new(TokenType::IDENT, "ten"),
            Test::new(TokenType::RPAREN, ")"),
            Test::new(TokenType::SEMICOLON, ";"),
            Test::new(TokenType::BANG, "!"),
            Test::new(TokenType::MINUS, "-"),
            Test::new(TokenType::SLASH, "/"),
            Test::new(TokenType::ASTERISK, "*"),
            Test::new(TokenType::INT, "5"),
            Test::new(TokenType::SEMICOLON, ";"),
            Test::new(TokenType::INT, "5"),
            Test::new(TokenType::LT, "<"),
            Test::new(TokenType::INT, "10"),
            Test::new(TokenType::GT, ">"),
            Test::new(TokenType::INT, "5"),
            Test::new(TokenType::SEMICOLON, ";"),
            Test::new(TokenType::IF, "if"),
            Test::new(TokenType::LPAREN, "("),
            Test::new(TokenType::INT, "5"),
            Test::new(TokenType::LT, "<"),
            Test::new(TokenType::INT, "10"),
            Test::new(TokenType::RPAREN, ")"),
            Test::new(TokenType::LBRACE, "{"),
            Test::new(TokenType::RETURN, "return"),
            Test::new(TokenType::TRUE, "true"),
            Test::new(TokenType::SEMICOLON, ";"),
            Test::new(TokenType::RBRACE, "}"),
            Test::new(TokenType::ELSE, "else"),
            Test::new(TokenType::LBRACE, "{"),
            Test::new(TokenType::RETURN, "return"),
            Test::new(TokenType::FALSE, "false"),
            Test::new(TokenType::SEMICOLON, ";"),
            Test::new(TokenType::RBRACE, "}"),
            Test::new(TokenType::INT, "10"),
            Test::new(TokenType::EQ, "=="),
            Test::new(TokenType::INT, "10"),
            Test::new(TokenType::SEMICOLON, ";"),
            Test::new(TokenType::INT, "10"),
            Test::new(TokenType::NEQ, "!="),
            Test::new(TokenType::INT, "9"),
            Test::new(TokenType::SEMICOLON, ";"),
            Test::new(TokenType::STRING, "foobar"),
            Test::new(TokenType::STRING, "foo bar"),
            Test::new(TokenType::LBRACKET, "["),
            Test::new(TokenType::INT, "1"),
            Test::new(TokenType::COMMA, ","),
            Test::new(TokenType::INT, "2"),
            Test::new(TokenType::RBRACKET, "]"),
            Test::new(TokenType::SEMICOLON, ";"),
            Test::new(TokenType::EOF, ""),
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