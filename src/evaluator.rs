use crate::object::ObjectLike;
use crate::{ast, object};

const NULL: Option<object::Object> = Some(object::Object::Null(object::Null{}));
const TRUE: Option<object::Object> = Some(object::Object::Boolean(object::Boolean{ value: true }));
const FALSE: Option<object::Object> = Some(object::Object::Boolean(object::Boolean{ value: false }));

pub(crate) fn eval(node: Option<ast::Node>) -> Option<object::Object> {
    match node {

        // Statements
        Some(ast::Node::Program(node)) =>
            eval_statements(node.statements),

        Some(ast::Node::Statement(ast::Statement::ExpressionStatement(node))) =>
            eval(node.expression.map(|e| ast::Node::Expression(e))),

        // Expressions
        Some(ast::Node::Expression(ast::Expression::PrefixExpression(node))) => {
            let right = eval(node.right.map(|e| ast::Node::Expression(*e)));
            eval_prefix_expression(node.operator.as_str(), right)
        }

        Some(ast::Node::Expression(ast::Expression::InfixExpression(node))) => {
            let left = eval(node.left.map(|e| ast::Node::Expression(*e)));
            let right = eval(node.right.map(|e| ast::Node::Expression(*e)));
            eval_infix_expression(node.operator.as_str(), left, right)
        }

        Some(ast::Node::Expression(ast::Expression::IntegerLiteral(node))) =>
            Some(object::Object::Integer(object::Integer{ value: node.value })),

        Some(ast::Node::Expression(ast::Expression::Boolean(node))) =>
            native_bool_to_boolean_object(node.value),

        _ => None
    }
}

fn native_bool_to_boolean_object(input: bool) -> Option<object::Object> {
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

fn eval_prefix_expression(operator: &str, right: Option<object::Object>) -> Option<object::Object> {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => NULL
    }
}

fn eval_bang_operator_expression(right: Option<object::Object>) -> Option<object::Object> {
    match right {
        TRUE => FALSE,
        FALSE => TRUE,
        NULL => TRUE,
        _ => FALSE,
    }
}

fn eval_minus_prefix_operator_expression(right: Option<object::Object>) -> Option<object::Object> {
    if right.as_ref().map(|r| r.object_type()) != Some(object::ObjectType::IntegerObj) {
        return NULL
    }

    let value = match right? {
        object::Object::Integer(inner) => inner.value,
        _ => panic!()
    };
    Some(object::Object::Integer(object::Integer{ value: -value }))
}

fn eval_infix_expression(operator: &str, left: Option<object::Object>, right: Option<object::Object>) -> Option<object::Object> {
    if left.as_ref()?.object_type() == object::ObjectType::IntegerObj && right.as_ref()?.object_type() == object::ObjectType::IntegerObj {
        return eval_integer_infix_expression(operator, left, right)
    }
    NULL
}

fn eval_integer_infix_expression(operator: &str, left: Option<object::Object>, right: Option<object::Object>) -> Option<object::Object> {
    let left_val = match left {
        Some(object::Object::Integer(inner)) => inner.value,
        _ => panic!()
    };
    let right_val = match right {
        Some(object::Object::Integer(inner)) => inner.value,
        _ => panic!()
    };

    match operator {
        "+" => Some(object::Object::Integer(object::Integer{ value: left_val + right_val })),
        "-" => Some(object::Object::Integer(object::Integer{ value: left_val - right_val })),
        "*" => Some(object::Object::Integer(object::Integer{ value: left_val * right_val })),
        "/" => Some(object::Object::Integer(object::Integer{ value: left_val / right_val })),
        "<" => native_bool_to_boolean_object(left_val < right_val),
        ">" => native_bool_to_boolean_object(left_val > right_val),
        "==" => native_bool_to_boolean_object(left_val == right_val),
        "!=" => native_bool_to_boolean_object(left_val != right_val),
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
            Test::new("-5", -5),
            Test::new("-10", -10),
		    Test::new("5 + 5 + 5 + 5 - 10", 10),
		    Test::new("2 * 2 * 2 * 2 * 2", 32),
		    Test::new("-50 + 100 + -50", 0),
		    Test::new("5 * 2 + 10", 20),
		    Test::new("5 + 2 * 10", 25),
		    Test::new("20 + 2 * -10", 0),
		    Test::new("50 / 2 * 2 + 10", 60),
		    Test::new("2 * (5 + 10)", 30),
		    Test::new("3 * 3 * 3 + 10", 37),
		    Test::new("3 * (3 * 3) + 10", 37),
		    Test::new("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
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
		    Test::new("1 < 2", true),
		    Test::new("1 > 2", false),
		    Test::new("1 < 1", false),
		    Test::new("1 > 1", false),
		    Test::new("1 == 1", true),
		    Test::new("1 != 1", false),
		    Test::new("1 == 2", false),
		    Test::new("1 != 2", true),
            Test::new("true == true", true),
            Test::new("false == false", true),
            Test::new("true == false", false),
            Test::new("true != false", true),
            Test::new("false != true", true),
            Test::new("(1 < 2) == true", true),
            Test::new("(1 < 2) == false", false),
            Test::new("(1 > 2) == true", false),
            Test::new("(1 > 2) == false", true),
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
                eprint!("object is not Boolean. got={:?}\n", obj);
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
    fn test_bang_operator() {
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
            Test::new("!true", false),
            Test::new("!false", true),
            Test::new("!5", false),
            Test::new("!!true", true),
            Test::new("!!false", false),
            Test::new("!!5", true),
        ];

        let mut should_panic = false;

        for tt in tests.into_iter() {
            let evaluated = test_eval(tt.input.as_str());
            if !test_boolean_object(evaluated, tt.expected) {
                should_panic = true
            }
        }

        if should_panic {
            panic!()
        }
    }
}