use crate::{ast, lexer, token};

struct Parser<'a> {
    l: &'a mut lexer::Lexer,

    cur_token: token::Token,
    peek_token: token::Token,
}

impl<'a> Parser<'a> {
    fn new(l: &'a mut lexer::Lexer) -> Self {
        let mut p = Parser{
            l,
            cur_token: Default::default(),
            peek_token: Default::default(),
        };

        // Read two tokens, so cur_token and peek_token are both set
        p.next_token();
        p.next_token();

        p
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program{ statements: vec![] };

        while self.cur_token.token_type != token::TokenType::EOF {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(ast::Statement::LetStatement(stmt))
            }
            self.next_token();
        }

        program
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

        let mut l = lexer::Lexer::new(input);
        let mut p = Parser::new(&mut l);

        let program = p.parse_program();

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
}