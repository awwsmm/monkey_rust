use crate::token;
use std::fmt::{Display, Formatter};

pub(crate) trait Node : Display {
    fn token_literal(&self) -> String;
}

#[derive(Clone, Debug)]
pub(crate) enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Statement::LetStatement(let_statement) => let_statement.to_string(),
            Statement::ReturnStatement(return_statement) => return_statement.to_string(),
        };
        write!(f, "{}", string)
    }
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::LetStatement(let_statement) => let_statement.token_literal(),
            Statement::ReturnStatement(return_statement) => return_statement.token_literal(),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Expression {
    Identifier(Identifier),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Expression::Identifier(identifier) => identifier.to_string(),
        };
        write!(f, "{}", string)
    }
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(identifier) => identifier.token_literal(),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Program {
    pub(crate) statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.statements.iter().map(|s| write!(f, "{}", s.to_string())).into_iter().collect()
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if let [head, ..] = &*self.statements {
            head.token_literal()
        } else {
            String::from("")
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct LetStatement {
    pub(crate) token: token::Token, // the token::TokenType::LET token
    pub(crate) name: Option<Identifier>,
    pub(crate) value: Option<Expression>,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = self.name.clone().map(|x| x.to_string()).unwrap_or(String::from(""));
        let value = self.value.clone().map(|x| format!("{}", x)).unwrap_or(String::from(""));
        write!(f, "{} {} = {};", self.token_literal(), name, value)
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Identifier {
    pub(crate) token: token::Token, // the token::TokenType::IDENT token
    pub(crate) value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct ReturnStatement {
    pub(crate) token: token::Token,
    pub(crate) return_value: Option<Expression>,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let return_value = self.return_value.clone().map(|x| format!(" {}", x));
        write!(f, "{}{};", self.token_literal(), return_value.unwrap_or(String::from("")))
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

struct ExpressionStatement {
    token: token::Token, // the first token of the expression
    expression: Expression,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression.to_string())
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}