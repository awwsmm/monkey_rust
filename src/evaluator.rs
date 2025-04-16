use crate::{ast, object};

const TRUE: object::Boolean = object::Boolean{ value: true };
const FALSE: object::Boolean = object::Boolean{ value: false };

pub(crate) fn eval(node: Option<ast::Node>) -> Option<object::Object> {
    match node {

        // Statements
        Some(ast::Node::Program(node)) =>
            eval_statements(node.statements),

        Some(ast::Node::Statement(ast::Statement::ExpressionStatement(node))) =>
            eval(node.expression.map(|e| ast::Node::Expression(e))),

        // Expressions
        Some(ast::Node::Expression(ast::Expression::IntegerLiteral(node))) =>
            Some(object::Object::Integer(object::Integer{ value: node.value })),

        Some(ast::Node::Expression(ast::Expression::Boolean(node))) =>
            Some(object::Object::Boolean(native_bool_to_boolean_object(node.value))),

        _ => None
    }
}

fn native_bool_to_boolean_object(input: bool) -> object::Boolean {
    if input {
        return TRUE
    }
    FALSE
}

fn eval_statements(stmts: Vec<ast::Statement>) -> Option<object::Object> {
    let mut result = None;

    for statement in stmts.into_iter() {
        result = eval(Some(ast::Node::Statement(statement)))
    }

    result
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

        eval(Some(ast::Node::Program(program)))
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

    #[test]
    fn test_eval_boolean_expression() {
        struct Test {
            input: String,
            expected: bool,
        }

        impl Test {
            fn new(input: &str, expected: bool) -> Self {
                Self { input: input.to_owned(), expected }
            }
        }

        let tests = vec![
            Test::new("true", true),
            Test::new("false", false),
        ];

        let mut should_panic = false;

        for tt in tests.iter() {
            let evaluated = test_eval(tt.input.as_str());
            if !test_boolean_object(evaluated, tt.expected) {
                should_panic = true
            }
        }

        if should_panic {
            panic!()
        }
    }

    fn test_boolean_object(obj: Option<object::Object>, expected: bool) -> bool {
        let result = match obj {
            Some(object::Object::Boolean(inner)) => inner,
            _ => {
                eprint!("object is not Boolean. got={:?}", obj);
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