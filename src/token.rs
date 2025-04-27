use std::fmt::{Display, Formatter};

#[derive(Default, Eq, PartialEq, Clone, Copy, Debug, Hash)]
pub(crate) enum TokenType {
    #[default]
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT, // add, foobar, x, y, ...
    INT, // 1343456
    STRING,

    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,

    LT,
    GT,

    EQ,
    NEQ,

    // Delimiters
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,

    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            TokenType::ILLEGAL => "ILLEGAL",
            TokenType::EOF => "EOF",

            TokenType::IDENT => "IDENT",
            TokenType::INT => "INT",
            TokenType::STRING => "STRING",

            TokenType::ASSIGN => "=",
            TokenType::PLUS => "+",
            TokenType::MINUS => "-",
            TokenType::BANG => "!",
            TokenType::ASTERISK => "*",
            TokenType::SLASH => "/",

            TokenType::LT => "<",
            TokenType::GT => ">",

            TokenType::EQ => "==",
            TokenType::NEQ => "!=",

            TokenType::COMMA => ",",
            TokenType::SEMICOLON => ";",

            TokenType::LPAREN => "(",
            TokenType::RPAREN => ")",
            TokenType::LBRACE => "{",
            TokenType::RBRACE => "}",
            TokenType::LBRACKET => "[",
            TokenType::RBRACKET => "]",

            TokenType::FUNCTION => "FUNCTION",
            TokenType::LET => "LET",
            TokenType::TRUE => "TRUE",
            TokenType::FALSE => "FALSE",
            TokenType::IF => "IF",
            TokenType::ELSE => "ELSE",
            TokenType::RETURN => "RETURN",
        };

        write!(f, "{}", string)
    }
}

impl TokenType {
    pub(crate) fn lookup_ident(ident: &str) -> Self {
        match ident {
            "fn" => Self::FUNCTION,
            "let" => Self::LET,
            "true" => Self::TRUE,
            "false" => Self::FALSE,
            "if" => Self::IF,
            "else" => Self::ELSE,
            "return" => Self::RETURN,
            _ => Self::IDENT
        }
    }
}

#[derive(Default, Clone, Debug, PartialEq)]
pub(crate) struct Token {
    pub(crate) token_type: TokenType,
    pub(crate) literal: String,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{Type:{} Literal:{}}}", self.token_type, self.literal)
    }
}

impl Token {
    pub(crate) fn new(token_type: TokenType, literal: String) -> Self {
        Self { token_type, literal }
    }
}
