trait Node {
    fn token_literal(&self) -> String;
}

trait Statement: Node {}

trait Expression: Node {}

struct Program {
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