use std::fmt::{Display, Formatter};

#[derive(Default, Eq, PartialEq)]
pub(crate) enum TokenType {
    #[default]
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

impl TokenType {
    fn lookup_ident(ident: &str) -> Self {
        match ident {
            "fn" => Self::FUNCTION,
            "let" => Self::LET,
            _ => Self::IDENT
        }
    }
}

#[derive(Default)]
pub(crate) struct Token {
    pub(crate) token_type: TokenType,
    pub(crate) literal: String,
}

impl Token {
    pub(crate) fn new(token_type: TokenType, literal: String) -> Self {
        Self { token_type, literal }
    }
}
