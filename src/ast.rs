use crate::token;
use std::fmt::{Display, Formatter};

pub(crate) trait Node : Display {
    fn token_literal(&self) -> &str;
}

#[derive(Clone, Debug)]
pub(crate) enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Statement::LetStatement(inner) => inner.to_string(),
            Statement::ReturnStatement(inner) => inner.to_string(),
            Statement::ExpressionStatement(inner) => inner.to_string(),
            Statement::BlockStatement(inner) => inner.to_string(),
        };
        write!(f, "{}", string)
    }
}

impl Node for Statement {
    fn token_literal(&self) -> &str {
        match self {
            Statement::LetStatement(inner) => inner.token_literal(),
            Statement::ReturnStatement(inner) => inner.token_literal(),
            Statement::ExpressionStatement(inner) => inner.token_literal(),
            Statement::BlockStatement(inner) => inner.token_literal(),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    Boolean(Boolean),
    IfExpression(IfExpression),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Expression::Identifier(inner) => inner.to_string(),
            Expression::IntegerLiteral(inner) => inner.to_string(),
            Expression::PrefixExpression(inner) => inner.to_string(),
            Expression::InfixExpression(inner) => inner.to_string(),
            Expression::Boolean(inner) => inner.to_string(),
            Expression::IfExpression(inner) => inner.to_string(),
        };
        write!(f, "{}", string)
    }
}

impl Node for Expression {
    fn token_literal(&self) -> &str {
        match self {
            Expression::Identifier(inner) => inner.token_literal(),
            Expression::IntegerLiteral(inner) => inner.token_literal(),
            Expression::PrefixExpression(inner) => inner.token_literal(),
            Expression::InfixExpression(inner) => inner.token_literal(),
            Expression::Boolean(inner) => inner.token_literal(),
            Expression::IfExpression(inner) => inner.token_literal(),
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
    fn token_literal(&self) -> &str {
        if let [head, ..] = &*self.statements {
            head.token_literal()
        } else {
            ""
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
        let name = self.name.as_ref().map(|x| x.to_string()).unwrap_or_default();
        let value = self.value.as_ref().map(|x| format!("{}", x)).unwrap_or_default();
        write!(f, "{} {} = {};", self.token_literal(), name, value)
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> &str {
        &*self.token.literal
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
    fn token_literal(&self) -> &str {
        &*self.token.literal
    }
}

#[derive(Clone, Debug)]
pub(crate) struct ReturnStatement {
    pub(crate) token: token::Token,
    pub(crate) return_value: Option<Expression>,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let return_value = self.return_value.as_ref().map(|x| format!(" {}", x));
        write!(f, "{}{};", self.token_literal(), return_value.unwrap_or_default())
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> &str {
        &*self.token.literal
    }
}

#[derive(Clone, Debug)]
pub(crate) struct ExpressionStatement {
    pub(crate) token: token::Token, // the first token of the expression
    pub(crate) expression: Option<Expression>,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression.as_ref().map(|x| x.to_string()).unwrap_or_default())
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> &str {
        &*self.token.literal
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
    fn token_literal(&self) -> &str {
        &*self.token.literal
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PrefixExpression {
    pub(crate) token: token::Token, // The prefix token, e.g. !
    pub(crate) operator: String,
    pub(crate) right: Option<Box<Expression>>,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right.as_ref().map(|x| x.to_string()).unwrap_or_default())
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> &str {
        &*self.token.literal
    }
}

#[derive(Clone, Debug)]
pub(crate) struct InfixExpression {
    pub(crate) token: token::Token, // The operator token, e.g. +
    pub(crate) left: Option<Box<Expression>>,
    pub(crate) operator: String,
    pub(crate) right: Option<Box<Expression>>,
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let left = self.left.as_ref().map(|x| x.to_string()).unwrap_or_default();
        let right = self.right.as_ref().map(|x| x.to_string()).unwrap_or_default();
        write!(f, "({} {} {})", left, self.operator, right)
    }
}

impl Node for InfixExpression {
    fn token_literal(&self) -> &str {
        &*self.token.literal
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Boolean {
    pub(crate) token: token::Token,
    pub(crate) value: bool,
}

impl Display for Boolean {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}

impl Node for Boolean {
    fn token_literal(&self) -> &str {
        &*self.token.literal
    }
}

#[derive(Clone, Debug)]
pub(crate) struct IfExpression {
    pub(crate) token: token::Token, // The 'if' token
    pub(crate) condition: Box<Expression>,
    pub(crate) consequence: BlockStatement,
    pub(crate) alternative: Option<BlockStatement>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let else_branch = self.alternative.as_ref().map(|e| format!("else {}", e.to_string())).unwrap_or_else(|| String::new());
        write!(f, "if {} {}{}", self.condition.to_string(), self.consequence.to_string(), else_branch)
    }
}

impl Node for IfExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

#[derive(Clone, Debug)]
pub(crate) struct BlockStatement {
    pub(crate) token: token::Token, // the { token
    pub(crate) statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut string = String::new();

        for s in self.statements.iter() {
            string.push_str(&s.to_string())
        }

        write!(f, "{}", string)
    }
}

impl Node for BlockStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
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