use crate::{ast, lexer, token};

struct Parser {
    l: lexer::Lexer,

    errors: Vec<String>,

    cur_token: token::Token,
    peek_token: token::Token,
}

impl Parser {
    fn new(l: lexer::Lexer) -> Self {
        let mut p = Parser{
            l,
            errors: vec![],
            cur_token: Default::default(),
            peek_token: Default::default(),
        };

        // Read two tokens, so cur_token and peek_token are both set
        p.next_token();
        p.next_token();

        p
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
            _ => None,
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Node, Statement};

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

        for stmt in program.statements.iter() {
            let return_statement = match stmt {
                Statement::ReturnStatement(rs) => rs,
                _ => {
                    eprint!("s not ast::ReturnStatement. got={:?}", stmt.clone());
                    continue
                },
            };
            if return_statement.token_literal() != "return" {
                eprint!("return_statement.token_literal() not 'return', got {}",
                    return_statement.token_literal())
            }
        }
    }
}