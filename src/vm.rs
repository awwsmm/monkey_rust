#[cfg(test)]
mod tests {
    use crate::{ast, lexer, object, parser};

    fn parse(input: &'static str) -> ast::Program {
        let l = lexer::Lexer::new(input);
        let mut p = parser::Parser::new(l);
        p.parse_program()
    }

    fn test_integer_object(expected: i32, actual: object::Object) -> Option<crate::compiler::Error> {
        let result = match actual {
            object::Object::IntegerObj(integer_obj) => integer_obj,
            _ => return crate::compiler::Error::new(format!(
                "object is not Integer. got={:?}", actual
            )),
        };

        if result.value != expected {
            return crate::compiler::Error::new(format!(
                "object has wrong value. got={}, want={}", result.value, expected
            ))
        }

        None
    }
}
