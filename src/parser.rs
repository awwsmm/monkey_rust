use crate::token::TokenType;
use crate::{ast, lexer, token};
use std::cmp::PartialOrd;
use std::collections::HashMap;

pub(crate) struct Parser {
    l: lexer::Lexer,
    pub(crate) errors: Vec<String>,

    cur_token: token::Token,
    peek_token: token::Token,

    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    pub(crate) fn new(l: lexer::Lexer) -> Self {
        let mut p = Parser{
            l,
            errors: vec![],
            cur_token: Default::default(),
            peek_token: Default::default(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        p.register_prefix(TokenType::IDENT, Self::parse_identifier);
        p.register_prefix(TokenType::INT, Self::parse_integer_literal);
        p.register_prefix(TokenType::BANG, Self::parse_prefix_expression);
        p.register_prefix(TokenType::MINUS, Self::parse_prefix_expression);
        p.register_prefix(TokenType::TRUE, Self::parse_boolean);
        p.register_prefix(TokenType::FALSE, Self::parse_boolean);
        p.register_prefix(TokenType::LPAREN, Self::parse_grouped_expression);
        p.register_prefix(TokenType::IF, Self::parse_if_expression);
        p.register_prefix(TokenType::FUNCTION, Self::parse_function_literal);
        p.register_prefix(TokenType::STRING, Self::parse_string_literal);
        p.register_prefix(TokenType::LBRACKET, Self::parse_array_literal);

        p.register_infix(TokenType::PLUS, Self::parse_infix_expression);
        p.register_infix(TokenType::MINUS, Self::parse_infix_expression);
        p.register_infix(TokenType::SLASH, Self::parse_infix_expression);
        p.register_infix(TokenType::ASTERISK, Self::parse_infix_expression);
        p.register_infix(TokenType::EQ, Self::parse_infix_expression);
        p.register_infix(TokenType::NEQ, Self::parse_infix_expression);
        p.register_infix(TokenType::LT, Self::parse_infix_expression);
        p.register_infix(TokenType::GT, Self::parse_infix_expression);
        p.register_infix(TokenType::LPAREN, Self::parse_call_expression);
        p.register_infix(TokenType::LBRACKET, Self::parse_index_expression);

        // Read two tokens, so cur_token and peek_token are both set
        p.next_token();
        p.next_token();

        p
    }

    fn parse_index_expression(&mut self, left: ast::Expression) -> Option<ast::Expression> {
        let mut exp = ast::IndexExpression{
            token: self.cur_token.clone(),
            left: Box::new(left),
            index: None,
        };

        self.next_token();
        exp.index = self.parse_expression(Precedence::Lowest).map(Box::new);

        if !self.expect_peek(TokenType::RBRACKET) {
            return None
        }

        Some(ast::Expression::IndexExpression(exp))
    }

    fn parse_array_literal(&mut self) -> Option<ast::Expression> {
        let mut array = ast::ArrayLiteral{
            token: self.cur_token.clone(),
            elements: vec![],
        };

        array.elements = self.parse_expression_list(TokenType::RBRACKET);

        Some(ast::Expression::ArrayLiteral(array))
    }

    fn parse_string_literal(&mut self) -> Option<ast::Expression> {
        Some(ast::Expression::StringLiteral(ast::StringLiteral{
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        }))
    }

    fn parse_call_expression(&mut self, function: ast::Expression) -> Option<ast::Expression> {
        let mut exp = ast::CallExpression{
            token: self.cur_token.clone(),
            function: Box::new(function),
            arguments: vec![],
        };
        exp.arguments = self.parse_expression_list(TokenType::RPAREN);
        Some(ast::Expression::CallExpression(exp))
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Vec<ast::Expression> {
        let mut list = vec![];

        if self.peek_token_is(end) {
            self.next_token();
            return list
        }

        self.next_token();
        list.push(self.parse_expression(Precedence::Lowest).unwrap());

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedence::Lowest).unwrap())
        }

        if !self.expect_peek(end) {
            return vec![]
        }

        list
    }

    fn parse_function_parameters(&mut self) -> Vec<ast::Identifier> {
        let mut identifiers = vec![];

        if self.peek_token_is(TokenType::RPAREN) {
            self.next_token();
            return identifiers
        }

        self.next_token();

        let ident = ast::Identifier{ token: self.cur_token.clone(), value: self.cur_token.literal.clone() };
        identifiers.push(ident);

        while self.peek_token_is(TokenType::COMMA) {
            self.next_token();
            self.next_token();
            let ident = ast::Identifier{ token: self.cur_token.clone(), value: self.cur_token.literal.clone() };
            identifiers.push(ident)
        }

        if !self.expect_peek(TokenType::RPAREN) {
            return vec![]
        }

        identifiers
    }

    fn parse_function_literal(&mut self) -> Option<ast::Expression> {
        let mut lit = ast::FunctionLiteral{
            token: self.cur_token.clone(),
            parameters: vec![],
            body: None,
        };

        if !self.expect_peek(TokenType::LPAREN) {
            return None
        }

        lit.parameters = self.parse_function_parameters();

        if !self.expect_peek(TokenType::LBRACE) {
            return None
        }

        lit.body = Some(self.parse_block_statement());

        Some(ast::Expression::FunctionLiteral(lit))
    }

    fn parse_block_statement(&mut self) -> ast::BlockStatement {
        let mut block = ast::BlockStatement{ token: self.cur_token.clone(), statements: vec![] };

        self.next_token();

        while !self.cur_token_is(TokenType::RBRACE) && !self.cur_token_is(TokenType::EOF) {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                block.statements.push(stmt)
            }
            self.next_token()
        }

        block
    }

    fn parse_if_expression(&mut self) -> Option<ast::Expression> {
        let mut expression = ast::IfExpression{
            token: self.cur_token.clone(),
            condition: None,
            consequence: None,
            alternative: None,
        };

        if !self.expect_peek(TokenType::LPAREN) {
            return None
        }

        self.next_token();
        expression.condition = self.parse_expression(Precedence::Lowest).map(Box::new);

        if !self.expect_peek(TokenType::RPAREN) {
            return None
        }

        if !self.expect_peek(TokenType::LBRACE) {
            return None
        }

        expression.consequence = Some(self.parse_block_statement());

        if self.peek_token_is(TokenType::ELSE) {
            self.next_token();

            if !self.expect_peek(TokenType::LBRACE) {
                return None
            }

            expression.alternative = Some(self.parse_block_statement())
        }

        Some(ast::Expression::IfExpression(expression))
    }

    fn parse_grouped_expression(&mut self) -> Option<ast::Expression> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(TokenType::RPAREN) {
            return None
        }

        exp
    }

    fn parse_identifier(&mut self) -> Option<ast::Expression> {
        Some(ast::Expression::Identifier(ast::Identifier{
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        }))
    }

    fn errors(&self) -> &Vec<String> {
        self.errors.as_ref()
    }

    fn peek_error(&mut self, t: TokenType) {
        let msg = format!("expected next token to be {}, got {} instead",
            t, self.peek_token.token_type);
        self.errors.push(msg)
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub(crate) fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program{ statements: vec![] };

        while self.cur_token.token_type != TokenType::EOF {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt)
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.cur_token.token_type {
            TokenType::LET => self.parse_let_statement().map(ast::Statement::LetStatement),
            TokenType::RETURN => self.parse_return_statement().map(ast::Statement::ReturnStatement),
            _ => self.parse_expression_statement().map(ast::Statement::ExpressionStatement),
        }
    }

    fn parse_let_statement(&mut self) -> Option<ast::LetStatement> {
        let mut stmt = ast::LetStatement{
            token: self.cur_token.clone(),
            name: None,
            value: None,
        };

        if !self.expect_peek(TokenType::IDENT) {
            return None
        }

        stmt.name = Some(ast::Identifier{
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        });

        if !self.expect_peek(TokenType::ASSIGN) {
            return None
        }

        self.next_token();

        stmt.value = self.parse_expression(Precedence::Lowest);

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token()
        }

        Some(stmt)
    }

    fn cur_token_is(&self, t: TokenType) -> bool {
        self.cur_token.token_type == t
    }

    fn peek_token_is(&self, t: TokenType) -> bool {
        self.peek_token.token_type == t
    }

    fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token_is(t.clone()) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }

    fn parse_return_statement(&mut self) -> Option<ast::ReturnStatement> {
        let mut stmt = ast::ReturnStatement{
            token: self.cur_token.clone(),
            return_value: None,
        };

        self.next_token();

        stmt.return_value = self.parse_expression(Precedence::Lowest);

        while !self.cur_token_is(TokenType::SEMICOLON) {
            self.next_token()
        }

        Some(stmt)
    }

    fn register_prefix(&mut self, token_type: TokenType, f: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, f);
    }

    fn register_infix(&mut self, token_type: TokenType, f: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, f);
    }

    fn parse_expression_statement(&mut self) -> Option<ast::ExpressionStatement> {
        let stmt = ast::ExpressionStatement{
            token: self.cur_token.clone(),
            expression: self.parse_expression(Precedence::Lowest),
        };

        if self.peek_token_is(TokenType::SEMICOLON) {
            self.next_token()
        }

        Some(stmt)
    }

    fn no_prefix_parse_fn_error(&mut self, t: TokenType) {
        let msg = format!("no prefix parse function for {} found", t);
        self.errors.push(msg)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<ast::Expression> {
        let prefix = self.prefix_parse_fns.get(&self.cur_token.token_type);
        if prefix.is_none() {
            self.no_prefix_parse_fn_error(self.cur_token.token_type.clone());
            return None
        }

        let mut left_exp = (*prefix.unwrap())(self);

        while !self.peek_token_is(TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            let infix = match self.infix_parse_fns.get(&self.peek_token.token_type) {
                None => return left_exp,
                Some(infix_parse_fn) => infix_parse_fn,
            }.clone();

            self.next_token();

            left_exp = left_exp.map(|l| infix(self, l)).flatten()
        }

        left_exp
    }

    fn parse_integer_literal(&mut self) -> Option<ast::Expression> {
        let mut lit = ast::IntegerLiteral{ token: self.cur_token.clone(), value: 0 };

        let value = match str::parse::<i32>(&*self.cur_token.literal) {
            Ok(integer_value) => integer_value,
            Err(_) => {
                let msg = format!("could not parse {} as integer", self.cur_token.literal);
                self.errors.push(msg);
                return None
            }
        };

        lit.value = value;

        Some(ast::Expression::IntegerLiteral(lit))
    }

    fn parse_prefix_expression(&mut self) -> Option<ast::Expression> {
        let mut expression = ast::PrefixExpression{
            token: self.cur_token.clone(),
            operator: self.cur_token.literal.to_string(),
            right: None,
        };

        self.next_token();

        expression.right = self.parse_expression(Precedence::Prefix).map(Box::new);

        Some(ast::Expression::PrefixExpression(expression))
    }

    fn parse_boolean(&mut self) -> Option<ast::Expression> {
        Some(ast::Expression::Boolean(ast::Boolean {
            token: self.cur_token.clone(),
            value: self.cur_token_is(TokenType::TRUE)
        }))
    }

    fn parse_infix_expression(&mut self, left: ast::Expression) -> Option<ast::Expression> {
        let mut expression = ast::InfixExpression{
            token: self.cur_token.clone(),
            left: Some(Box::new(left)),
            operator: self.cur_token.literal.clone(),
            right: None,
        };

        let precedence = self.cur_precedence();
        self.next_token();
        expression.right = self.parse_expression(precedence).map(Box::new);

        Some(ast::Expression::InfixExpression(expression))
    }

    fn peek_precedence(&self) -> Precedence {
        let thing = PRECEDENCES.iter().find_map(|(token_type, precedence)| {
            if token_type == &self.peek_token.token_type {
                Some(precedence)
            } else {
                None
            }
        });

        match thing {
            None => Precedence::Lowest,
            Some(precedence) => *precedence,
        }
    }

    fn cur_precedence(&self) -> Precedence {
        let thing = PRECEDENCES.iter().find_map(|(token_type, precedence)| {
            if token_type == &self.cur_token.token_type {
                Some(precedence)
            } else {
                None
            }
        });

        match thing {
            None => Precedence::Lowest,
            Some(precedence) => *precedence,
        }
    }
}

#[derive(Clone, Copy, PartialOrd, PartialEq)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
    Index,       // array[index]
}

const PRECEDENCES: [(TokenType, Precedence); 10] = [
    (TokenType::EQ, Precedence::Equals),
    (TokenType::NEQ, Precedence::Equals),
    (TokenType::LT, Precedence::LessGreater),
    (TokenType::GT, Precedence::LessGreater),
    (TokenType::PLUS, Precedence::Sum),
    (TokenType::MINUS, Precedence::Sum),
    (TokenType::SLASH, Precedence::Product),
    (TokenType::ASTERISK, Precedence::Product),
    (TokenType::LPAREN, Precedence::Call),
    (TokenType::LBRACKET, Precedence::Index),
];

type PrefixParseFn = fn(&mut Parser) -> Option<ast::Expression>;
type InfixParseFn = fn(&mut Parser, ast::Expression) -> Option<ast::Expression>;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;
    use std::any::type_name_of_val;

    fn test_integer_literal(il: &Expression, value: i32) -> bool {
        let integ = match il {
            Expression::IntegerLiteral(integer_literal) => integer_literal,
            _ => {
                eprint!("il not ast::IntegerLiteral. got={}", type_name_of_val(&il));
                return false
            }
        };

        if integ.value != value {
            eprint!("integ.value not {}. got={}\n", value, integ.value);
            return false
        }

        if integ.token_literal() != format!("{}", value) {
            eprint!("integ.token_literal() not {}. got={}\n", value, integ.token_literal());
            return false
        }

        true
    }

    fn test_identifier(exp: &Expression, value: &str) -> bool {
        let ident = match exp {
            Expression::Identifier(identifier) => identifier,
            _ => {
                eprint!("exp not ast::Identifier. got={}", type_name_of_val(&exp));
                return false
            }
        };

        if ident.value != value {
            eprint!("ident.value not {}. got={}\n", value, ident.value);
            return false
        }

        if ident.token_literal() != format!("{}", value) {
            eprint!("ident.token_literal() not {}. got={}\n", value, ident.token_literal())
        }

        true
    }

    fn test_boolean_literal(exp: &Expression, value: bool) -> bool {
        let bo = match exp {
            Expression::Boolean(boolean) => boolean,
            _ => {
                eprint!("exp not ast::Boolean. got={}", type_name_of_val(&exp));
                return false
            }
        };

        if bo.value != value {
            eprint!("bo.value not {}. got={}\n", value, bo.value);
            return false
        }

        if bo.token_literal() != format!("{}", value) {
            eprint!("bo.token_literal() not {}. got={}\n", value, bo.token_literal());
            return false
        }

        true
    }

    enum Expected {
        IntegerLiteral(i32),
        Identifier(String),
        BooleanLiteral(bool),
    }

    impl From<i32> for Expected {
        fn from(value: i32) -> Self {
            Self::IntegerLiteral(value)
        }
    }

    impl From<&str> for Expected {
        fn from(value: &str) -> Self {
            Self::Identifier(value.to_owned())
        }
    }

    impl From<bool> for Expected {
        fn from(value: bool) -> Self {
            Self::BooleanLiteral(value)
        }
    }

    fn test_literal_expression(exp: &Expression, expected: impl Into<Expected>) -> bool {
        match expected.into() {
            Expected::IntegerLiteral(v) => test_integer_literal(exp, v),
            Expected::Identifier(v) => test_identifier(exp, v.as_str()),
            Expected::BooleanLiteral(v) => test_boolean_literal(exp, v),
        }
    }

    fn test_infix_expression(exp: &Expression, left: impl Into<Expected>, operator: &str, right: impl Into<Expected>) -> bool {
        let exp = match exp {
            Expression::InfixExpression(infix_expression) => infix_expression,
            _ => {
                eprint!("exp not ast::InfixExpression. got={}", type_name_of_val(&exp));
                return false
            }
        };

        if !test_literal_expression(exp.left.as_ref().unwrap(), left) {
            return false
        };

        if exp.operator != operator {
            eprint!("exp.operator is not '{}', got={}", operator, exp.operator)
        }

        if !test_literal_expression(exp.right.as_ref().unwrap(), right) {
            return false
        }

        true
    }

    fn check_parser_errors(p: Parser) {
        let errors = p.errors();
        if errors.len() == 0 {
            return
        }

        eprint!("parser has {} errors\n", errors.len());
        for msg in errors.iter() {
            eprint!("parser error: {}\n", msg)
        }
        panic!()
    }

    fn test_let_statement(s: &Statement, name: &str) -> bool {
        if s.token_literal() != "let" {
            eprint!("s.token_literal not 'let'. got={}", s.token_literal());
            return false
        }

        let let_statement = match s {
            Statement::LetStatement(ls) => ls,
            _ => {
                eprint!("s not ast::LetStatement. got={:?}", s);
                return false
            },
        };

        let let_statement_name = let_statement.name.as_ref().unwrap();

        if let_statement_name.value != name {
            eprint!("let_statement.name.value not '{}'. got={}", name, let_statement_name.value);
            return false
        };

        if let_statement_name.token_literal() != name {
            eprint!("let_statement.name.token_literal() not '{}'. got={}",
                    name, let_statement_name.token_literal());
            return false
        };

        true
    }

    #[test]
    fn test_let_statements() {
        struct Test {
            input: String,
            expected_identifier: String,
            expected_value: Expected,
        }

        impl Test {
            fn new(input: &str, expected_identifier: &str, expected_value: impl Into<Expected>) -> Self {
                Self {
                    input: input.to_owned(),
                    expected_identifier: expected_identifier.to_owned(),
                    expected_value: expected_value.into(),
                }
            }
        }

        let tests = vec![
            Test::new("let x = 5;", "x", 5),
            Test::new("let y = true;", "y", true),
            Test::new("let foobar = y;", "foobar", "y"),
        ];

        for tt in tests.into_iter() {
            let l = lexer::Lexer::new(tt.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(p);

            if program.statements.len() != 1 {
                panic!("program.statements does not contain 1 statements. got={}",
                       program.statements.len());
            }

            let stmt = program.statements.get(0).unwrap();
            if !test_let_statement(stmt, tt.expected_identifier.as_str()) {
                panic!()
            }

            let val = match stmt {
                Statement::LetStatement(inner) => inner.value.as_ref().unwrap(),
                _ => panic!(),
            };
            if !test_literal_expression(val, tt.expected_value) {
                panic!()
            }
        }
    }

    #[test]
    fn test_return_statements() {
        struct Test {
            input: String,
            expected_value: Expected,
        }

        impl Test {
            fn new(input: &str, expected_value: impl Into<Expected>) -> Self {
                Self {
                    input: input.to_owned(),
                    expected_value: expected_value.into(),
                }
            }
        }

        let tests = vec![
            Test::new("return 5;", 5),
            Test::new("return true;", true),
            Test::new("return foobar;", "foobar"),
        ];

        for tt in tests.into_iter() {
            let l = lexer::Lexer::new(tt.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(p);

            if program.statements.len() != 1 {
                panic!("program.statements does not contain 1 statements. got={}",
                       program.statements.len());
            }

            let stmt = program.statements.get(0).unwrap();
            let return_stmt = match stmt {
                Statement::ReturnStatement(inner) => inner,
                _ => panic!("stmt not ast::ReturnStatement. got={}", stmt)
            };

            if return_stmt.token_literal() != "return" {
                panic!("return_stmt.token_literal() not 'return', got {}",
                return_stmt.token_literal())
            }

            if !test_literal_expression(return_stmt.return_value.as_ref().unwrap(), tt.expected_value) {
                panic!()
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        if program.statements.len() != 1 {
            panic!("program.statements has not enough statements. got={}",
                   program.statements.len());
        }

        let stmt = match program.statements.into_iter().next().unwrap() {
            Statement::ExpressionStatement(expression_statement) => expression_statement,
            unexpected => panic!("program.statements.get(0) is not ast::ExpressionStatement. got={}",
                        type_name_of_val(&unexpected)),
        };

        if !test_literal_expression(&stmt.expression.unwrap(), "foobar") {
            panic!()
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        if program.statements.len() != 1 {
            panic!("program has not enough statements. got={}",
                   program.statements.len());
        }

        let stmt = match program.statements.into_iter().next().unwrap() {
            Statement::ExpressionStatement(expression_statement) => expression_statement,
            unexpected => panic!("program.statements.get(0) is not ast::ExpressionStatement. got={}",
                        type_name_of_val(&unexpected)),
        };

        if !test_literal_expression(&stmt.expression.unwrap(), 5) {
            panic!()
        }
    }

    #[test]
    fn test_boolean_expression() {
        let input = "true;";

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        if program.statements.len() != 1 {
            panic!("program has not enough statements. got={}",
                   program.statements.len());
        }

        let stmt = match program.statements.into_iter().next().unwrap() {
            Statement::ExpressionStatement(expression_statement) => expression_statement,
            unexpected => panic!("program.statements.get(0) is not ast::ExpressionStatement. got={}",
                                 type_name_of_val(&unexpected)),
        };

        if !test_literal_expression(&stmt.expression.unwrap(), true) {
            panic!()
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        struct Test {
            input: String,
            operator: String,
            value: Expected,
        }

        impl Test {
            fn new(input: &str, operator: &str, value: impl Into<Expected>) -> Self {
                Self {
                    input: input.to_owned(),
                    operator: operator.to_string(),
                    value: value.into(),
                }
            }
        }

        let prefix_tests = vec![
            Test::new("!5", "!", 5),
            Test::new("-15", "-", 15),
            Test::new("!true;", "!", true),
            Test::new("!false;", "!", false),
        ];

        for tt in prefix_tests.into_iter() {
            let l = lexer::Lexer::new(tt.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(p);

            if program.statements.len() != 1 {
                panic!("program.statements does not contain {} statements. got={}",
                       1, program.statements.len());
            }

            let stmt = match program.statements.get(0).unwrap() {
                Statement::ExpressionStatement(expression_statement) => expression_statement,
                _ => panic!("program.statements.get(0) is not ast::ExpressionStatement. got={}",
                            type_name_of_val(program.statements.get(0).unwrap())),
            };

            let exp = match stmt.expression.as_ref() {
                Some(Expression::PrefixExpression(prefix_expression)) => prefix_expression,
                _ => panic!("exp not ast::PrefixExpression. got={:?}", type_name_of_val(&stmt.expression))
            };

            if exp.operator != tt.operator {
                panic!("exp.operator is not '{}', got={}",
                    tt.operator, exp.operator)
            }

            if !test_literal_expression(exp.right.as_ref().unwrap(), tt.value) {
                panic!()
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        struct Test {
            input: String,
            left_value: Expected,
            operator: String,
            right_value: Expected,
        }

        impl Test {
            fn new(input: &str, left_value: impl Into<Expected>, operator: &str, right_value: impl Into<Expected>) -> Self {
                Self {
                    input: input.to_owned(),
                    left_value: left_value.into(),
                    operator: operator.to_owned(),
                    right_value: right_value.into(),
                }
            }
        }

        let infix_tests = vec![
            Test::new("5 + 5", 5, "+", 5),
            Test::new("5 - 5", 5, "-", 5),
            Test::new("5 * 5", 5, "*", 5),
            Test::new("5 / 5", 5, "/", 5),
            Test::new("5 > 5", 5, ">", 5),
            Test::new("5 < 5", 5, "<", 5),
            Test::new("5 == 5", 5, "==", 5),
            Test::new("5 != 5", 5, "!=", 5),
            Test::new("true == true", true, "==", true),
            Test::new("true != false", true, "!=", false),
            Test::new("false == false", false, "==", false),
        ];

        for tt in infix_tests.into_iter() {
            let l = lexer::Lexer::new(tt.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(p);

            if program.statements.len() != 1 {
                panic!("program.statements does not contain {} statements. got={}",
                       1, program.statements.len());
            }

            let stmt = match program.statements.into_iter().next().unwrap() {
                Statement::ExpressionStatement(expression_statement) => expression_statement,
                unexpected => panic!("program.statements.get(0) is not ast::ExpressionStatement. got={}",
                            type_name_of_val(&unexpected)),
            };

            if !test_infix_expression(&stmt.expression.unwrap(), tt.left_value, tt.operator.as_str(), tt.right_value) {
                panic!()
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        struct Test {
            input: String,
            expected: String,
        }

        impl Test {
            fn new(input: &str, expected: &str) -> Self {
                Self {
                    input: input.to_owned(),
                    expected: expected.to_owned(),
                }
            }
        }

        let tests = vec![
            Test::new(
                "-a * b",
                "((-a) * b)"
            ),
            Test::new(
                "!-a",
                "(!(-a))"
            ),
            Test::new(
                "a + b + c",
                "((a + b) + c)"
            ),
            Test::new(
                "a + b - c",
                "((a + b) - c)"
            ),
            Test::new(
                "a * b * c",
                "((a * b) * c)"
            ),
            Test::new(
                "a * b / c",
                "((a * b) / c)"
            ),
            Test::new(
                "a + b / c",
                "(a + (b / c))"
            ),
            Test::new(
                "a + b * c + d / e - f",
                "(((a + (b * c)) + (d / e)) - f)"
            ),
            Test::new(
                "3 + 4; -5 * 5;",
                "(3 + 4)((-5) * 5)"
            ),
            Test::new(
                "5 > 4 == 3 < 4",
                "((5 > 4) == (3 < 4))"
            ),
            Test::new(
                "5 < 4 != 3 > 4",
                "((5 < 4) != (3 > 4))"
            ),
            Test::new(
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"
            ),
            Test::new(
                "true",
                "true"
            ),
            Test::new(
                "false",
                "false"
            ),
            Test::new(
                "3 > 5 == false",
                "((3 > 5) == false)"
            ),
            Test::new(
                "3 < 5 == true",
                "((3 < 5) == true)"
            ),
            Test::new(
                "1 + (2 + 3) + 4",
                "((1 + (2 + 3)) + 4)"
            ),
            Test::new(
                "(5 + 5) * 2",
                "((5 + 5) * 2)"
            ),
            Test::new(
                "2 / (5 + 5)",
                "(2 / (5 + 5))"
            ),
            Test::new(
                "-(5 + 5)",
                "(-(5 + 5))"
            ),
            Test::new(
                "!(true == true)",
                "(!(true == true))"
            ),
            Test::new(
                "a + add(b * c) + d",
                "((a + add((b * c))) + d)",
            ),
            Test::new(
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            Test::new(
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            Test::new(
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            Test::new(
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        let mut should_panic = false;

        for tt in tests.into_iter() {
            let l = lexer::Lexer::new(tt.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(p);

            let actual = program.to_string();
            if actual != tt.expected {
                eprint!("expected={}, got={}\n", tt.expected, actual);
                should_panic = true
            }
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        if program.statements.len() != 1 {
            panic!("program.statements does not contain {} statements. got={}",
                   1, program.statements.len());
        }

        let stmt = match program.statements.get(0).unwrap() {
            Statement::ExpressionStatement(expression_statement) => expression_statement,
            _ => panic!("program.statements.get(0) is not ast::ExpressionStatement. got={}",
                        type_name_of_val(program.statements.get(0).unwrap())),
        };

        let exp = match stmt.expression.as_ref() {
            Some(Expression::IfExpression(if_expression)) => if_expression,
            _ => panic!("exp not ast::IfExpression. got={:?}", type_name_of_val(&stmt.expression))
        };

        if !test_infix_expression(exp.condition.as_ref().unwrap(), "x", "<", "y") {
            panic!()
        }

        let mut should_panic = false;

        if exp.consequence.as_ref().unwrap().statements.len() != 1 {
            should_panic = true;
            eprint!("consequence is not 1 statements. got={}\n",
                exp.consequence.as_ref().unwrap().statements.len())
        }

        let consequence = match exp.consequence.as_ref().unwrap().statements.get(0).unwrap() {
            Statement::ExpressionStatement(expression_statement) => expression_statement,
            _ => panic!("statements.get(0) is not ast::ExpressionStatement. got={}",
                        type_name_of_val(exp.consequence.as_ref().unwrap().statements.get(0).unwrap())),
        };

        if !test_identifier(consequence.expression.as_ref().unwrap(), "x") {
            panic!()
        }

        if exp.alternative.is_some() {
            should_panic = true;
            eprint!("exp.alternative was not None. got={:?}", exp.alternative)
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        if program.statements.len() != 1 {
            panic!("program.statements does not contain {} statements. got={}",
                   1, program.statements.len());
        }

        let stmt = match program.statements.get(0).unwrap() {
            Statement::ExpressionStatement(expression_statement) => expression_statement,
            _ => panic!("program.statements.get(0) is not ast::ExpressionStatement. got={}",
                        type_name_of_val(program.statements.get(0).unwrap())),
        };

        let exp = match stmt.expression.as_ref() {
            Some(Expression::IfExpression(if_expression)) => if_expression,
            _ => panic!("exp not ast::IfExpression. got={:?}", type_name_of_val(&stmt.expression))
        };

        if !test_infix_expression(&exp.condition.as_ref().unwrap(), "x", "<", "y") {
            panic!()
        }

        let mut should_panic = false;

        if exp.consequence.as_ref().unwrap().statements.len() != 1 {
            should_panic = true;
            eprint!("consequence is not 1 statements. got={}\n",
                    exp.consequence.as_ref().unwrap().statements.len())
        }

        let consequence = match exp.consequence.as_ref().unwrap().statements.get(0).unwrap() {
            Statement::ExpressionStatement(expression_statement) => expression_statement,
            _ => panic!("statements.get(0) is not ast::ExpressionStatement. got={}",
                        type_name_of_val(exp.consequence.as_ref().unwrap().statements.get(0).unwrap())),
        };

        if !test_identifier(consequence.expression.as_ref().unwrap(), "x") {
            panic!()
        }

        if exp.alternative.as_ref().unwrap().statements.len() != 1 {
            should_panic = true;
            eprint!("alternative is not 1 statements. got={}\n",
                    exp.alternative.as_ref().unwrap().statements.len())
        }

        let alternative = match exp.alternative.as_ref().unwrap().statements.get(0).unwrap() {
            Statement::ExpressionStatement(expression_statement) => expression_statement,
            _ => panic!("statements.get(0) is not ast::ExpressionStatement. got={}",
                        type_name_of_val(exp.alternative.as_ref().unwrap().statements.get(0).unwrap())),
        };

        if !test_identifier(alternative.expression.as_ref().unwrap(), "y") {
            panic!()
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        if program.statements.len() != 1 {
            panic!("program.statements does not contain {} statements. got={}",
                   1, program.statements.len());
        }

        let stmt = match program.statements.get(0).unwrap() {
            Statement::ExpressionStatement(expression_statement) => expression_statement,
            _ => panic!("program.statements.get(0) is not ast::ExpressionStatement. got={}",
                        type_name_of_val(program.statements.get(0).unwrap())),
        };

        let function = match stmt.expression.as_ref() {
            Some(Expression::FunctionLiteral(if_expression)) => if_expression,
            _ => panic!("exp not ast::IfExpression. got={:?}", type_name_of_val(&stmt.expression))
        };

        if function.parameters.len() != 2 {
            panic!("function literal parameters wrong. want 2, got={}\n",
            function.parameters.len())
        }

        test_literal_expression(&Expression::Identifier(function.parameters.get(0).unwrap().clone()), "x");
        test_literal_expression(&Expression::Identifier(function.parameters.get(1).unwrap().clone()), "y");

        if function.body.as_ref().unwrap().statements.len() != 1 {
            panic!("function.body.statements has not 1 statements. got={}\n",
                   function.body.as_ref().unwrap().statements.len())
        }

        let body_stmt = match function.body.as_ref().unwrap().statements.get(0).unwrap() {
            Statement::ExpressionStatement(expression_statement) => expression_statement,
            _ => panic!("function body stmt is not ast::ExpressionStatement. got={}",
                        type_name_of_val(function.body.as_ref().unwrap().statements.get(0).unwrap())),
        };

        test_infix_expression(body_stmt.expression.as_ref().unwrap(), "x", "=", "y");
    }

    #[test]
    fn test_function_parameter_parsing() {
        struct Test {
            input: String,
            expected_params: Vec<String>,
        }

        impl Test {
            fn new(input: &str, expected_params: Vec<&str>) -> Self {
                Self {
                    input: input.to_owned(),
                    expected_params: expected_params.into_iter().map(|s| s.to_owned()).collect(),
                }
            }
        }

        let tests = vec![
            Test::new("fn() {};", vec![]),
            Test::new("fn(x) {};", vec!["x"]),
            Test::new("fn(x, y, z) {};", vec!["x", "y", "z"]),
        ];

        let mut should_panic = false;

        for tt in tests.into_iter() {
            let l = lexer::Lexer::new(tt.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(p);

            let stmt = match program.statements.get(0) {
                Some(Statement::ExpressionStatement(inner)) => inner,
                _ => panic!(),
            };

            let function = match stmt.expression.as_ref() {
                Some(Expression::FunctionLiteral(inner)) => inner,
                _ => panic!(),
            };

            if function.parameters.len() != tt.expected_params.len() {
                should_panic = true;
                eprint!("length parameters wrong. want {}, got={}\n",
                        tt.expected_params.len(), function.parameters.len())
            }

            for (i, ident) in tt.expected_params.iter().enumerate() {
                if !test_literal_expression(&Expression::Identifier(function.parameters.get(i).unwrap().to_owned()), ident.as_str()) {
                    should_panic = true
                }
            }
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5)";

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        if program.statements.len() != 1 {
            panic!("program.statements does not contain {} statements. got={}\n",
                   1, program.statements.len());
        }

        let stmt = match program.statements.get(0).unwrap() {
            Statement::ExpressionStatement(expression_statement) => expression_statement,
            _ => panic!("stmt is not ast::ExpressionStatement. got={}",
                        type_name_of_val(program.statements.get(0).unwrap())),
        };

        let exp = match stmt.expression.as_ref() {
            Some(Expression::CallExpression(inner)) => inner,
            _ => panic!("stmt.expression not ast::CallExpression. got={:?}", type_name_of_val(&stmt.expression))
        };

        if !test_identifier(exp.function.as_ref(), "add") {
            return
        }

        if exp.arguments.len() != 3 {
            panic!("wrong length of arguments. got={}", exp.arguments.len())
        }

        test_literal_expression(exp.arguments.get(0).unwrap(), 1);
        test_infix_expression(exp.arguments.get(1).unwrap(), 2, "*", 3);
        test_infix_expression(exp.arguments.get(2).unwrap(), 4, "+", 5);
    }

    #[test]
    fn test_call_expression_parameter_parsing() {
        struct Test {
            input: String,
            expected_ident: String,
            expected_args: Vec<String>,
        }

        impl Test {
            fn new(input: &str, expected_ident: &str, expected_args: Vec<&str>) -> Self {
                Self {
                    input: input.to_owned(),
                    expected_ident: expected_ident.to_owned(),
                    expected_args: expected_args.into_iter().map(|s| s.to_owned()).collect()
                }
            }
        }

        let tests = vec![
            Test::new(
                "add();",
                "add",
                vec![]
            ),
            Test::new(
                "add(1);",
                "add",
                vec!["1"]
            ),
            Test::new(
                "add(1, 2 * 3, 4 + 5);",
                "add",
                vec!["1", "(2 * 3)", "(4 + 5)"]
            ),
        ];

        let mut should_panic = false;

        for tt in tests.into_iter() {
            let l = lexer::Lexer::new(tt.input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(p);

            let stmt = match program.statements.get(0) {
                Some(Statement::ExpressionStatement(inner)) => inner,
                _ => panic!(),
            };

            let exp = match stmt.expression.as_ref() {
                Some(Expression::CallExpression(inner)) => inner,
                _ => panic!("stmt.expression is not ast::Expression::CallExpression. got={}",
                        type_name_of_val(stmt.expression.as_ref().unwrap())),
            };

            if !test_identifier(exp.function.as_ref(), tt.expected_ident.as_str()) {
                panic!()
            }

            if exp.arguments.len() != tt.expected_args.len() {
                panic!("wrong number of arguments. want={}, got={}",
                       tt.expected_args.len(), exp.arguments.len()
                )
            }

            for (i, arg) in tt.expected_args.iter().enumerate() {
                if exp.arguments.get(i).unwrap().to_string() != arg.to_owned() {
                    should_panic = true;
                    eprint!("argument {} wrong. want={}, got={}", i, arg, exp.arguments.get(i).unwrap().to_string())
                }
            }
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_string_literal_expression() {
        let input = r#""hello world";"#;

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        let stmt = match program.statements.get(0) {
            Some(Statement::ExpressionStatement(inner)) => inner,
            _ => panic!()
        };

        let literal = match stmt.expression.as_ref() {
            Some(Expression::StringLiteral(inner)) => inner,
            _ => panic!("exp not ast::StringLiteral. got={:?}", stmt.expression)
        };

        if literal.value != "hello world" {
            panic!("literal.value not {}. got={}", "hello world", literal.value)
        }
    }

    #[test]
    fn test_parsing_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        let stmt = match program.statements.get(0) {
            Some(Statement::ExpressionStatement(inner)) => inner,
            _ => panic!()
        };

        let array = match stmt.expression.as_ref() {
            Some(Expression::ArrayLiteral(inner)) => inner,
            _ => panic!("exp not ast::ArrayLiteral. got={:?}", stmt.expression)
        };

        if array.elements.len() != 3 {
            panic!("array.elements.len() not 3. got={}", array.elements.len())
        }

        let mut should_panic = false;

        if !test_integer_literal(array.elements.get(0).unwrap(), 1) {
            should_panic = true
        }

        if !test_infix_expression(array.elements.get(1).unwrap(), 2, "*", 2) {
            should_panic = true
        }

        if !test_infix_expression(array.elements.get(2).unwrap(), 3, "+", 3) {
            should_panic = true
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_parsing_empty_array_literals() {
        let input = "[]";

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        let stmt = match program.statements.get(0) {
            Some(Statement::ExpressionStatement(inner)) => inner,
            _ => panic!()
        };

        let array = match stmt.expression.as_ref() {
            Some(Expression::ArrayLiteral(inner)) => inner,
            _ => panic!("exp not ast::ArrayLiteral. got={:?}", stmt.expression)
        };

        if array.elements.len() != 0 {
            panic!("array.elements.len() not 0. got={}", array.elements.len())
        }
    }

    #[test]
    fn test_parsing_index_expressions() {
        let input = "myArray[1 + 1]";

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        let stmt = match program.statements.get(0) {
            Some(Statement::ExpressionStatement(inner)) => inner,
            _ => panic!()
        };

        let index_exp = match stmt.expression.as_ref() {
            Some(Expression::IndexExpression(inner)) => inner,
            _ => panic!("exp not ast::IndexExpression. got={:?}", stmt.expression)
        };

        if !test_identifier(&index_exp.left, "myArray") {
            panic!()
        }

        if !test_infix_expression(index_exp.index.as_ref().unwrap(), 1, "+", 1) {
            panic!()
        }
    }

    #[test]
    fn test_parsing_hash_literals_string_keys() {
        let input = r#"{"one": 1, "two": 2, "three": 3}"#;

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        let stmt = match program.statements.get(0) {
            Some(Statement::ExpressionStatement(inner)) => inner,
            _ => panic!()
        };

        let hash = match stmt.expression.as_ref() {
            Some(Expression::HashLiteral(inner)) => inner,
            _ => panic!("exp not ast::HashLiteral. got={:?}", stmt.expression)
        };

        let mut should_panic = false;

        if hash.pairs.len() != 3 {
            should_panic = true;
            eprintln!("hash.pairs has wrong length. got={}", hash.pairs.len())
        }

        let mut expected = HashMap::new();
        expected.insert("one", 1);
        expected.insert("two", 2);
        expected.insert("three", 3);

        for (key, value) in hash.pairs.iter() {
            let literal = match key {
                Some(Expression::StringLiteral(inner)) => inner,
                _ => panic!("key is not ast::StringLiteral. got={:?}", key)
            };

            let expected_value = expected.get(literal.to_string().as_str()).cloned().unwrap();

            if !test_integer_literal(value, expected_value) {
                should_panic = true
            }
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_parsing_empty_hash_literal() {
        let input = "{}";

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        let stmt = match program.statements.get(0) {
            Some(Statement::ExpressionStatement(inner)) => inner,
            _ => panic!()
        };

        let hash = match stmt.expression.as_ref() {
            Some(Expression::HashLiteral(inner)) => inner,
            _ => panic!("exp not ast::HashLiteral. got={:?}", stmt.expression)
        };

        if hash.pairs.len() != 0 {
            panic!("hash.pairs has wrong length. got={}", hash.pairs.len())
        }
    }

    #[test]
    fn test_parsing_hash_literals_integer_keys() {
        let input = r#"{6: 1, 5: 2, 4: 3}"#;

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        let stmt = match program.statements.get(0) {
            Some(Statement::ExpressionStatement(inner)) => inner,
            _ => panic!()
        };

        let hash = match stmt.expression.as_ref() {
            Some(Expression::HashLiteral(inner)) => inner,
            _ => panic!("exp not ast::HashLiteral. got={:?}", stmt.expression)
        };

        let mut should_panic = false;

        if hash.pairs.len() != 3 {
            should_panic = true;
            eprintln!("hash.pairs has wrong length. got={}", hash.pairs.len())
        }

        let mut expected = HashMap::new();
        expected.insert(6, 1);
        expected.insert(5, 2);
        expected.insert(4, 3);

        for (key, value) in hash.pairs.iter() {
            let literal = match key {
                Some(Expression::IntegerLiteral(inner)) => inner,
                _ => panic!("key is not ast::IntegerLiteral. got={:?}", key)
            };

            let expected_value = expected.get(&literal.value).cloned().unwrap();

            if !test_integer_literal(value, expected_value) {
                should_panic = true
            }
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_parsing_hash_literals_boolean_keys() {
        let input = r#"{true: 1, false: 2}"#;

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        let stmt = match program.statements.get(0) {
            Some(Statement::ExpressionStatement(inner)) => inner,
            _ => panic!()
        };

        let hash = match stmt.expression.as_ref() {
            Some(Expression::HashLiteral(inner)) => inner,
            _ => panic!("exp not ast::HashLiteral. got={:?}", stmt.expression)
        };

        let mut should_panic = false;

        if hash.pairs.len() != 2 {
            should_panic = true;
            eprintln!("hash.pairs has wrong length. got={}", hash.pairs.len())
        }

        let mut expected = HashMap::new();
        expected.insert(true, 1);
        expected.insert(false, 2);

        for (key, value) in hash.pairs.iter() {
            let literal = match key {
                Some(Expression::Boolean(inner)) => inner,
                _ => panic!("key is not ast::Boolean. got={:?}", key)
            };

            let expected_value = expected.get(&literal.value).cloned().unwrap();

            if !test_integer_literal(value, expected_value) {
                should_panic = true
            }
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_parsing_hash_literals_with_expressions() {
        let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        let stmt = match program.statements.get(0) {
            Some(Statement::ExpressionStatement(inner)) => inner,
            _ => panic!()
        };

        let hash = match stmt.expression.as_ref() {
            Some(Expression::HashLiteral(inner)) => inner,
            _ => panic!("exp not ast::HashLiteral. got={:?}", stmt.expression)
        };

        let mut should_panic = false;

        if hash.pairs.len() != 3 {
            should_panic = true;
            eprintln!("hash.pairs has wrong length. got={}", hash.pairs.len())
        }

        let mut tests = HashMap::<dyn Into<String>, fn(&Expression) -> bool>::new();
        tests.insert("one", |e| test_infix_expression(e, 0, "+", 1));
        tests.insert("two", |e| test_infix_expression(e, 10, "-", 8));
        tests.insert("three", |e| test_infix_expression(e, 15, "/", 5));

        for (key, value) in hash.pairs.iter() {
            let literal = match key {
                Some(Expression::StringLiteral(inner)) => inner,
                _ => panic!("key is not ast::StringLiteral. got={:?}", key)
            };

            let test_func = match tests.get(literal) {
                Some(value) => value,
                None => {
                    should_panic = true;
                    eprintln!("No test function for key {} found", literal.to_string());
                    continue
                }
            };

            if !test_func(value) {
                should_panic = true
            }
        }

        if should_panic {
            panic!()
        }
    }
}