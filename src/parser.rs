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

    fn parse_program() -> &'a ast::Program {
        unimplemented!()
    }
}
