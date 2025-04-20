use crate::token;
use std::fmt::{Display, Formatter};

pub(crate) enum Node {
    Program(Program),
    Statement(Statement),
    Expression(Expression),
}

pub(crate) trait NodeLike: Display {
    fn token_literal(&self) -> &str;
}

trait DisplayableNode: Display + NodeLike {}
impl<T: Display + NodeLike> DisplayableNode for T {}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
}

impl Statement {
    fn inner(&self) -> Box<&dyn DisplayableNode> {
        match self {
            Statement::LetStatement(inner) => Box::new(inner),
            Statement::ReturnStatement(inner) => Box::new(inner),
            Statement::ExpressionStatement(inner) => Box::new(inner),
            Statement::BlockStatement(inner) => Box::new(inner),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner())
    }
}

impl NodeLike for Statement {
    fn token_literal(&self) -> &str {
        self.inner().token_literal()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    Boolean(Boolean),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
}

impl Expression {
    fn inner(&self) -> Box<&dyn DisplayableNode> {
        match self {
            Expression::Identifier(inner) => Box::new(inner),
            Expression::IntegerLiteral(inner) => Box::new(inner),
            Expression::PrefixExpression(inner) => Box::new(inner),
            Expression::InfixExpression(inner) => Box::new(inner),
            Expression::Boolean(inner) => Box::new(inner),
            Expression::IfExpression(inner) => Box::new(inner),
            Expression::FunctionLiteral(inner) => Box::new(inner),
            Expression::CallExpression(inner) => Box::new(inner),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner())
    }
}

impl NodeLike for Expression {
    fn token_literal(&self) -> &str {
        self.inner().token_literal()
    }
}

pub(crate) struct Program {
    pub(crate) statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.statements.iter().map(|s| write!(f, "{}", s.to_string())).into_iter().collect()
    }
}

impl NodeLike for Program {
    fn token_literal(&self) -> &str {
        if let [head, ..] = &*self.statements {
            head.token_literal()
        } else {
            ""
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
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

impl NodeLike for LetStatement {
    fn token_literal(&self) -> &str {
        &*self.token.literal
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Identifier {
    pub(crate) token: token::Token, // the token::TokenType::IDENT token
    pub(crate) value: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl NodeLike for Identifier {
    fn token_literal(&self) -> &str {
        &*self.token.literal
    }
}

#[derive(Clone, Debug, PartialEq)]
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

impl NodeLike for ReturnStatement {
    fn token_literal(&self) -> &str {
        &*self.token.literal
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct ExpressionStatement {
    pub(crate) token: token::Token, // the first token of the expression
    pub(crate) expression: Option<Expression>,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression.as_ref().map(|x| x.to_string()).unwrap_or_default())
    }
}

impl NodeLike for ExpressionStatement {
    fn token_literal(&self) -> &str {
        &*self.token.literal
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct IntegerLiteral {
    pub(crate) token: token::Token,
    pub(crate) value: i32,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}

impl NodeLike for IntegerLiteral {
    fn token_literal(&self) -> &str {
        &*self.token.literal
    }
}

#[derive(Clone, Debug, PartialEq)]
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

impl NodeLike for PrefixExpression {
    fn token_literal(&self) -> &str {
        &*self.token.literal
    }
}

#[derive(Clone, Debug, PartialEq)]
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

impl NodeLike for InfixExpression {
    fn token_literal(&self) -> &str {
        &*self.token.literal
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Boolean {
    pub(crate) token: token::Token,
    pub(crate) value: bool,
}

impl Display for Boolean {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}

impl NodeLike for Boolean {
    fn token_literal(&self) -> &str {
        &*self.token.literal
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct IfExpression {
    pub(crate) token: token::Token, // The 'if' token
    pub(crate) condition: Option<Box<Expression>>,
    pub(crate) consequence: Option<BlockStatement>,
    pub(crate) alternative: Option<BlockStatement>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let else_branch = self.alternative.as_ref().map(|e| format!("else {}", e.to_string())).unwrap_or_else(|| String::new());
        write!(f, "if {} {}{}", self.condition.as_ref().unwrap().to_string(), self.consequence.as_ref().unwrap().to_string(), else_branch)
    }
}

impl NodeLike for IfExpression {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

#[derive(Clone, Debug, PartialEq)]
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

impl NodeLike for BlockStatement {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct FunctionLiteral {
    pub(crate) token: token::Token, // the 'fn' token
    pub(crate) parameters: Vec<Identifier>,
    pub(crate) body: Option<BlockStatement>
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut params = vec![];

        for s in self.parameters.iter() {
            params.push(s.to_string())
        }

        let body = self.body.as_ref().map(|b| b.to_string()).unwrap_or_else(|| String::from(""));

        write!(f, "{}( {}) {}", self.token_literal(), params.join(", "), body)
    }
}

impl NodeLike for FunctionLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct CallExpression {
    pub(crate) token: token::Token, // The '(' token
    pub(crate) function: Box<Expression>, // Identifier or FunctionLiteral
    pub(crate) arguments: Vec<Expression>,
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut args = vec![];

        for a in self.arguments.iter() {
            args.push(a.to_string())
        }

        write!(f, "{}({})", self.function, args.join(", "))
    }
}

impl NodeLike for CallExpression {
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