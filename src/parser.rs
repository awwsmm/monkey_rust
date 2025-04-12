use crate::{ast, lexer, token};
use std::collections::HashMap;

struct Parser {
    l: lexer::Lexer,
    errors: Vec<String>,

    cur_token: token::Token,
    peek_token: token::Token,

    prefix_parse_fns: HashMap<token::TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<token::TokenType, InfixParseFn>,
}

impl Parser {
    fn new(l: lexer::Lexer) -> Self {
        let mut p = Parser{
            l,
            errors: vec![],
            cur_token: Default::default(),
            peek_token: Default::default(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        p.register_prefix(token::TokenType::IDENT, Parser::parse_identifier);

        // Read two tokens, so cur_token and peek_token are both set
        p.next_token();
        p.next_token();

        p
    }

    fn parse_identifier(&mut self) -> Option<ast::Expression> {
        Some(
           ast::Expression::Identifier(
               ast::Identifier{
                   token: self.cur_token.clone(),
                   value: self.cur_token.literal.clone(),
               }
           )
        )
    }

    fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn peek_error(&mut self, t: token::TokenType) {
        let msg = format!("expected next token to be {}, got {} instead",
            t, self.peek_token.token_type);
        self.errors.push(msg)
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program{ statements: vec![] };

        while self.cur_token.token_type != token::TokenType::EOF {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt)
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.cur_token.token_type {
            token::TokenType::LET => self.parse_let_statement().map(|x| ast::Statement::LetStatement(x)),
            token::TokenType::RETURN => self.parse_return_statement().map(|x| ast::Statement::ReturnStatement(x)),
            _ => self.parse_expression_statement().map(|x| ast::Statement::ExpressionStatement(x)),
        }
    }

    fn parse_let_statement(&mut self) -> Option<ast::LetStatement> {
        let mut stmt = ast::LetStatement{
            token: self.cur_token.clone(),
            name: None,
            value: None,
        };

        if !self.expect_peek(token::TokenType::IDENT) {
            return None
        }

        stmt.name = Some(ast::Identifier{
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        });

        if !self.expect_peek(token::TokenType::ASSIGN) {
            return None
        }

        // TODO: We're skipping the expressions until we
        // encounter a semicolon
        while !self.cur_token_is(token::TokenType::SEMICOLON) {
            self.next_token()
        }

        Some(stmt)
    }

    fn cur_token_is(&self, t: token::TokenType) -> bool {
        self.cur_token.token_type == t
    }

    fn peek_token_is(&self, t: token::TokenType) -> bool {
        self.peek_token.token_type == t
    }

    fn expect_peek(&mut self, t: token::TokenType) -> bool {
        if self.peek_token_is(t.clone()) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }

    fn parse_return_statement(&mut self) -> Option<ast::ReturnStatement> {
        let stmt = ast::ReturnStatement{
            token: self.cur_token.clone(),
            return_value: None,
        };

        self.next_token();

        // TODO we're skipping the expressions until we
        // encounter a semicolon
        while !self.cur_token_is(token::TokenType::SEMICOLON) {
            self.next_token()
        }

        Some(stmt)
    }

    fn register_prefix(&mut self, token_type: token::TokenType, f: PrefixParseFn) {
        self.prefix_parse_fns.insert(token_type, f);
    }

    fn register_infix(&mut self, token_type: token::TokenType, f: InfixParseFn) {
        self.infix_parse_fns.insert(token_type, f);
    }

    fn parse_expression_statement(&mut self) -> Option<ast::ExpressionStatement> {
        let stmt = ast::ExpressionStatement{
            token: self.cur_token.clone(),
            expression: self.parse_expression(Precedence::Lowest),
        };

        if self.peek_token_is(token::TokenType::SEMICOLON) {
            self.next_token()
        }

        Some(stmt)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<ast::Expression> {
        let prefix = self.prefix_parse_fns.get(&self.cur_token.token_type);
        if prefix.is_none() {
            return None
        }

        (*prefix.unwrap())(self)
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
}

enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

type PrefixParseFn = fn(&mut Parser) -> Option<ast::Expression>;
type InfixParseFn = fn(&Parser, ast::Expression) -> ast::Expression;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;
    use std::any::type_name_of_val;

    #[test]
    fn test_let_statements() {
        let input = String::from("
let x = 5;
let y = 10;
let foobar = 838383;
");

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(p);

        if program.statements.len() != 3 {
            panic!("program.statements does not contain 3 statements. got={}",
                program.statements.len());
        }

        struct Test {
            expected_identifier: String
        }

        impl Test {
            fn new(s: &str) -> Self {
                Self { expected_identifier: String::from(s) }
            }
        }

        let tests = vec![
            Test::new("x"),
            Test::new("y"),
            Test::new("foobar"),
        ];

        for (i, tt) in tests.iter().enumerate() {
            let stmt = program.statements.get(i).unwrap();
            if !test_let_statement(stmt.clone(), tt.expected_identifier.clone()) {
                panic!()
            }
        }
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

    fn test_let_statement(s: Statement, name: String) -> bool {
        if s.token_literal() != "let" {
            eprint!("s.token_literal not 'let'. got={}", s.token_literal());
            return false
        }

        let let_statement = match s {
            Statement::LetStatement(ls) => ls,
            _ => {
                eprint!("s not ast::LetStatement. got={:?}", s.clone());
                return false
            },
        };

        if let_statement.name.clone().unwrap().value != name {
            eprint!("let_statement.name.value not '{}'. got={}", name, let_statement.name.unwrap().value);
            return false
        };

        if let_statement.name.clone().unwrap().token_literal() != name {
            eprint!("let_statement.name.token_literal() not '{}'. got={}",
                    name, let_statement.name.unwrap().token_literal());
            return false
        };

        true
    }

    #[test]
    fn test_return_statements() {
        let input = String::from("
return 5;
return 10;
return 993322;
");

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(p);

        if program.statements.len() != 3 {
            panic!("program.statements does not contain 3 statements. got={}",
                   program.statements.len());
        }

        let mut should_panic = false;

        for stmt in program.statements.iter() {
            let return_statement = match stmt {
                Statement::ReturnStatement(rs) => rs,
                _ => {
                    eprint!("s not ast::ReturnStatement. got={}", type_name_of_val(stmt));
                    should_panic = true;
                    continue
                },
            };
            if return_statement.token_literal() != "return" {
                eprint!("return_statement.token_literal() not 'return', got {}",
                    return_statement.token_literal());
                should_panic = true;
            }
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = String::from("foobar;");

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        if program.statements.len() != 1 {
            panic!("program.statements has not enough statements. got={}",
                   program.statements.len());
        }

        let stmt = match program.statements.get(0).unwrap() {
            Statement::ExpressionStatement(expression_statement) => expression_statement,
            _ => panic!("program.statements.get(0) is not ast::ExpressionStatement. got={}",
                        type_name_of_val(program.statements.get(0).unwrap())),
        };

        let ident = match stmt.expression.clone() {
            Some(Expression::Identifier(identifier)) => identifier,
            _ => panic!("exp not ast::Identifier. got={:?}", type_name_of_val(&stmt.expression))
        };

        let mut should_panic = false;

        if ident.value != "foobar" {
            should_panic = true;
            eprint!("ident.value not {}. got={}\n", "foobar", ident.value)
        }

        if ident.token_literal() != "foobar" {
            should_panic = true;
            eprint!("ident.token_literal() not {}. got={}\n", "foobar", ident.token_literal())
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = String::from("5;");

        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(p);

        if program.statements.len() != 1 {
            panic!("program has not enough statements. got={}",
                   program.statements.len());
        }

        let stmt = match program.statements.get(0).unwrap() {
            Statement::ExpressionStatement(expression_statement) => expression_statement,
            _ => panic!("program.statements.get(0) is not ast::ExpressionStatement. got={}",
                        type_name_of_val(program.statements.get(0).unwrap())),
        };

        let literal = match stmt.expression.clone() {
            Some(Expression::IntegerLiteral(integer_literal)) => integer_literal,
            _ => panic!("exp not ast::IntegerLiteral. got={:?}", type_name_of_val(&stmt.expression))
        };

        let mut should_panic = false;

        if literal.value != 5 {
            should_panic = true;
            eprint!("literal.value not {}. got={}\n", 5, literal.value)
        }

        if literal.token_literal() != "5" {
            should_panic = true;
            eprint!("literal.token_literal() not {}. got={}\n", "5", literal.token_literal())
        }

        if should_panic {
            panic!()
        }
    }
}