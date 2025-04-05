trait Node {
    fn token_literal(&self) -> String;
}

trait Statement: Node {}

trait Expression: Node {}