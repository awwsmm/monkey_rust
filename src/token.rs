use std::fmt::{Display, Formatter};

#[derive(Eq, PartialEq)]
pub(crate) enum TokenType {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT, // add, foobar, x, y, ...
    INT, // 1343456

    // Operators
    ASSIGN,
    PLUS,

    // Delimiters
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // Keywords
    FUNCTION,
    LET,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            TokenType::ILLEGAL => "ILLEGAL",
            TokenType::EOF => "EOF",

            TokenType::IDENT => "IDENT",
            TokenType::INT => "INT",

            TokenType::ASSIGN => "=",
            TokenType::PLUS => "+",

            TokenType::COMMA => ",",
            TokenType::SEMICOLON => ";",

            TokenType::LPAREN => "(",
            TokenType::RPAREN => ")",
            TokenType::LBRACE => "{",
            TokenType::RBRACE => "}",

            TokenType::FUNCTION => "FUNCTION",
            TokenType::LET => "LET",
        };

        write!(f, "{}", string)
    }
}

impl From<u8> for TokenType {
    fn from(value: u8) -> Self {
        match value {
            0 => TokenType::EOF,

            b'=' => TokenType::ASSIGN,
            b'+' => TokenType::PLUS,

            b',' => TokenType::COMMA,
            b';' => TokenType::SEMICOLON,

            b'(' => TokenType::LPAREN,
            b')' => TokenType::RPAREN,
            b'{' => TokenType::LBRACE,
            b'}' => TokenType::RBRACE,

            _ => unimplemented!()
        }
    }
}

pub(crate) struct Token {
    pub(crate) token_type: TokenType,
    pub(crate) literal: String,
}

impl Token {
    pub(crate) fn new(token_type: TokenType, ch: u8) -> Self {
        let literal = match ch {
            0 => String::from(""),
            _ => String::from_utf8(vec![ch]).unwrap()
        };

        Self { token_type, literal }
    }
}
