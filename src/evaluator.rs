use crate::{ast, object};

fn eval(node: ast::Node) -> Option<object::Object> {
    match node {
        ast::Node::Expression(ast::Expression::IntegerLiteral(inner)) => {
            Some(object::Object::Integer(object::Integer{ value: inner.value }))
        }
        _ => None
    }

}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer, object, parser};

    #[test]
    fn test_eval_integer_expression() {
        struct Test {
            input: String,
            expected: i32,
        }

        impl Test {
            fn new(input: &str, expected: i32) -> Self {
                Self { input: input.to_owned(), expected }
            }
        }

        let tests = vec![
            Test::new("5", 5),
            Test::new("10", 10),
        ];

        let mut should_panic = false;

        for tt in tests.iter() {
            let evaluated = test_eval(tt.input.as_str());
            if !test_integer_object(evaluated, tt.expected) {
                should_panic = true
            }
        }

        if should_panic {
            panic!()
        }
    }

    fn test_eval(input: &str) -> Option<object::Object> {
        let l = lexer::Lexer::new(input);
        let mut p = parser::Parser::new(l);
        let program = p.parse_program();

        eval(ast::Node::Program(program))
    }

    fn test_integer_object(obj: Option<object::Object>, expected: i32) -> bool {
        let result = match obj {
            Some(object::Object::Integer(inner)) => inner,
            _ => {
                eprint!("object is not Integer. got={:?}", obj);
                return false
            }
        };
        if result.value != expected {
            eprint!("object has wrong value. got={}, want={}",
                    result.value, expected);
            return false
        }

        true
    }
}