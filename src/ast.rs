use crate::token;

trait Node {
    fn token_literal(&self) -> String;
}

trait Statement: Node {}

trait Expression: Node {}

pub(crate) struct Program {
    statements: Vec<Box<dyn Statement>>
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements.iter().next().map(|s| (*s).token_literal()).unwrap()
        } else {
            String::from("")
        }
    }
}

struct LetStatement<'a> {
    token: token::Token, // the token::TokenType::LET token
    name: &'a Identifier,
    value: dyn Expression,
}

impl Node for LetStatement<'_> {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Statement for LetStatement<'_> {}

struct Identifier {
    token: token::Token, // the token::TokenType::IDENT token
    value: String
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for Identifier {}