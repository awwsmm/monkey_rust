use crate::token;
use std::fmt::{Display, Formatter};

pub(crate) trait Node : Display {
    fn token_literal(&self) -> String;
}

#[derive(Clone, Debug)]
pub(crate) enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Statement::LetStatement(let_statement) => let_statement.to_string(),
            Statement::ReturnStatement(return_statement) => return_statement.to_string(),
            Statement::ExpressionStatement(expression_statement) => expression_statement.to_string(),
        };
        write!(f, "{}", string)
    }
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::LetStatement(let_statement) => let_statement.token_literal(),
            Statement::ReturnStatement(return_statement) => return_statement.token_literal(),
            Statement::ExpressionStatement(expression_statement) => expression_statement.token_literal(),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Expression::Identifier(identifier) => identifier.to_string(),
            Expression::IntegerLiteral(integer_literal) => integer_literal.to_string(),
            Expression::PrefixExpression(prefix_expression) => prefix_expression.to_string(),
            Expression::InfixExpression(infix_expression) => infix_expression.to_string(),
        };
        write!(f, "{}", string)
    }
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(identifier) => identifier.token_literal(),
            Expression::IntegerLiteral(integer_literal) => integer_literal.token_literal(),
            Expression::PrefixExpression(prefix_expression) => prefix_expression.token_literal(),
            Expression::InfixExpression(infix_expression) => infix_expression.token_literal(),
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

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
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

#[derive(Clone, Debug)]
pub(crate) struct ExpressionStatement {
    pub(crate) token: token::Token, // the first token of the expression
    pub(crate) expression: Option<Expression>,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression.clone().map(|x| x.to_string()).unwrap_or(String::from("")))
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct IntegerLiteral {
    pub(crate) token: token::Token,
    pub(crate) value: i32,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PrefixExpression {
    pub(crate) token: token::Token, // The prefix token, e.g. !
    pub(crate) operator: String,
    pub(crate) right: Box<Option<Expression>>,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right.clone().map(|x| x.to_string()).unwrap_or(String::from("")))
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct InfixExpression {
    pub(crate) token: token::Token, // The operator token, e.g. +
    pub(crate) left: Box<Option<Expression>>,
    pub(crate) operator: String,
    pub(crate) right: Box<Option<Expression>>,
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let left = self.left.clone().map(|x| x.to_string()).unwrap_or(String::from(""));
        let right = self.right.clone().map(|x| x.to_string()).unwrap_or(String::from(""));
        write!(f, "({} {} {})", left, self.operator, right)
    }
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string() {
        let program = Program{
            statements: vec![
                Statement::LetStatement(LetStatement{
                    token: token::Token{
                        token_type: token::TokenType::LET,
                        literal: String::from("let"),
                    },
                    name: Some(
                        Identifier{
                            token: token::Token{
                                token_type: token::TokenType::IDENT,
                                literal: String::from("myVar"),
                            },
                            value: String::from("myVar"),
                        }
                    ),
                    value: Some(
                        Expression::Identifier(
                            Identifier{
                                token: token::Token{
                                    token_type: token::TokenType::IDENT,
                                    literal: String::from("anotherVar"),
                                },
                                value: String::from("anotherVar"),
                            }
                        )
                    ),
                })
            ],
        };

        if program.to_string() != "let myVar = anotherVar;" {
            panic!("program.to_string() wrong. got={}", program.to_string())
        }
    }
}