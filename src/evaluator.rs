use crate::object::{IsError, ObjectLike};
use crate::{ast, object};

const NULL: Option<object::Object> = Some(object::Object::Null(object::Null{}));
const TRUE: Option<object::Object> = Some(object::Object::Boolean(object::Boolean{ value: true }));
const FALSE: Option<object::Object> = Some(object::Object::Boolean(object::Boolean{ value: false }));

pub(crate) fn eval(node: Option<ast::Node>, env: &mut object::environment::Environment) -> Option<object::Object> {
    match node {

        // Statements
        Some(ast::Node::Program(node)) =>
            eval_program(node, env),

        Some(ast::Node::Statement(ast::Statement::ExpressionStatement(node))) =>
            eval(node.expression.map(|e| ast::Node::Expression(e)), env),

        Some(ast::Node::Statement(ast::Statement::BlockStatement(node))) =>
            eval_block_statement(node, env),

        Some(ast::Node::Statement(ast::Statement::ReturnStatement(node))) => {
            let val = eval(node.return_value.map(|x| ast::Node::Expression(x)), env);
            if val.is_error() {
                return val
            }
            Some(object::Object::ReturnValue(object::ReturnValue{ value: val.map(|x| Box::new(x)) }))
        }

        Some(ast::Node::Statement(ast::Statement::LetStatement(node))) => {
            let val = eval(node.value.map(|x| ast::Node::Expression(x)), env);
            if val.is_error() {
                return val
            }
            env.set(node.name?.value.as_str(), val?);
            None
        }

        // Expressions
        Some(ast::Node::Expression(ast::Expression::PrefixExpression(node))) => {
            let right = eval(node.right.map(|e| ast::Node::Expression(*e)), env);
            if right.is_error() {
                return right
            }
            eval_prefix_expression(node.operator.as_str(), right)
        }

        Some(ast::Node::Expression(ast::Expression::InfixExpression(node))) => {
            let left = eval(node.left.map(|e| ast::Node::Expression(*e)), env);
            if left.is_error() {
                return left
            }

            let right = eval(node.right.map(|e| ast::Node::Expression(*e)), env);
            if right.is_error() {
                return right
            }

            eval_infix_expression(node.operator.as_str(), left, right)
        }

        Some(ast::Node::Expression(ast::Expression::IfExpression(node))) =>
            eval_if_expression(node, env),

        Some(ast::Node::Expression(ast::Expression::IntegerLiteral(node))) =>
            Some(object::Object::Integer(object::Integer{ value: node.value })),

        Some(ast::Node::Expression(ast::Expression::Boolean(node))) =>
            native_bool_to_boolean_object(node.value),

        Some(ast::Node::Expression(ast::Expression::Identifier(node))) =>
            eval_identifier(node, env),

        _ => None
    }
}

fn eval_identifier(node: ast::Identifier, env: &object::environment::Environment) -> Option<object::Object> {
    match env.get(node.value.as_str()) {
        None => object::Error::new(format!("identifier not found: {}", node.value)),
        Some(val) => Some(val)
    }
}

fn eval_program(program: ast::Program, env: &mut object::environment::Environment) -> Option<object::Object> {
    let mut result: Option<object::Object> = None;

    for statement in program.statements.into_iter() {
        result = eval(Some(ast::Node::Statement(statement)), env);

        if let Some(object::Object::ReturnValue(return_value)) = result {
            return return_value.value.map(|x| *x)

        } else if matches!(result, Some(object::Object::Error(_))) {
            return result
        }
    }

    result
}

fn eval_if_expression(ie: ast::IfExpression, env: &mut object::environment::Environment) -> Option<object::Object> {
    let condition = eval(ie.condition.map(|e| ast::Node::Expression(*e)) ,env);
    if condition.is_error() {
        return condition
    }

    if is_truthy(condition) {
        eval(ie.consequence.map(|x| ast::Node::Statement(ast::Statement::BlockStatement(x))), env)
    } else if ie.alternative.is_some() {
        eval(ie.alternative.map(|x| ast::Node::Statement(ast::Statement::BlockStatement(x))), env)
    } else {
        NULL
    }
}

fn is_truthy(obj: Option<object::Object>) -> bool {
    match obj {
        NULL => false,
        TRUE => true,
        FALSE => false,
        _ => true,
    }
}

fn native_bool_to_boolean_object(input: bool) -> Option<object::Object> {
    if input {
        return TRUE
    }
    FALSE
}

fn eval_block_statement(block: ast::BlockStatement, env: &mut object::environment::Environment) -> Option<object::Object> {
    let mut result: Option<object::Object> = None;

    for statement in block.statements.into_iter() {
        result = eval(Some(ast::Node::Statement(statement)), env);

        if result.is_some() {
            let rt = result.as_ref()?.object_type();
            if rt == object::ObjectType::ReturnValueObj || rt == object::ObjectType::ErrorObj {
                return result
            }
        }
    }

    result
}

fn eval_prefix_expression(operator: &str, right: Option<object::Object>) -> Option<object::Object> {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => object::Error::new(format!("unknown operator: {}{:?}", operator, right.as_ref().map(|x| x.object_type())))
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
    let right_type = right.as_ref().map(|x| x.object_type());
    if right_type != Some(object::ObjectType::IntegerObj) {
        return object::Error::new(format!("unknown operator: -{:?}", right_type))
    }

    let value = match right? {
        object::Object::Integer(inner) => inner.value,
        _ => panic!()
    };
    Some(object::Object::Integer(object::Integer{ value: -value }))
}

fn eval_infix_expression(operator: &str, left: Option<object::Object>, right: Option<object::Object>) -> Option<object::Object> {
    let left_type = left.as_ref().map(|x| x.object_type());
    let right_type = right.as_ref().map(|x| x.object_type());

    if left_type == Some(object::ObjectType::IntegerObj) && right_type == Some(object::ObjectType::IntegerObj) {
        eval_integer_infix_expression(operator, left, right)

    } else if operator == "==" {
        native_bool_to_boolean_object(left == right)

    } else if operator == "!=" {
        native_bool_to_boolean_object(left != right)

    } else if left_type != right_type {
        object::Error::new(format!("type mismatch: {:?} {} {:?}", left_type, operator, right_type))

    } else {
        object::Error::new(format!("unknown operator: {:?} {} {:?}", left_type, operator, right_type))
    }
}

fn eval_integer_infix_expression(operator: &str, left: Option<object::Object>, right: Option<object::Object>) -> Option<object::Object> {
    let left_type = left.as_ref().map(|x| x.object_type());
    let right_type = right.as_ref().map(|x| x.object_type());

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
        _ => object::Error::new(format!("unknown operator: {:?} {} {:?}", left_type, operator, right_type))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::Object;
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

        for tt in tests.into_iter() {
            let evaluated = test_eval(tt.input);
            if !test_integer_object(evaluated, tt.expected) {
                should_panic = true
            }
        }

        if should_panic {
            panic!()
        }
    }

    fn test_eval(input: impl Into<String>) -> Option<object::Object> {
        let l = lexer::Lexer::new(input);
        let mut p = parser::Parser::new(l);
        let program = p.parse_program();
        let mut env = object::environment::Environment::new();

        eval(Some(ast::Node::Program(program)), &mut env)
    }

    fn test_integer_object(obj: Option<object::Object>, expected: i32) -> bool {
        let result = match obj {
            Some(object::Object::Integer(inner)) => inner,
            _ => {
                eprint!("object is not Integer. got={:?}\n", obj);
                return false
            }
        };
        if result.value != expected {
            eprint!("object has wrong value. got={}, want={}\n",
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

        for tt in tests.into_iter() {
            let evaluated = test_eval(tt.input);
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
            eprint!("object has wrong value. got={}, want={}\n",
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
            let evaluated = test_eval(tt.input);
            if !test_boolean_object(evaluated, tt.expected) {
                should_panic = true
            }
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_if_else_expressions() {
        struct Test {
            input: String,
            expected: Option<i32>,
        }

        impl Test {
            fn new(input: &str, expected: Option<i32>) -> Self {
                Self { input: input.to_owned(), expected }
            }
        }

        let tests = vec![
            Test::new("if (true) { 10 }", Some(10)),
            Test::new("if (false) { 10 }", None),
            Test::new("if (1) { 10 }", Some(10)),
            Test::new("if (1 < 2) { 10 }", Some(10)),
            Test::new("if (1 > 2) { 10 }", None),
            Test::new("if (1 > 2) { 10 } else { 20 }", Some(20)),
            Test::new("if (1 < 2) { 10 } else { 20 }", Some(10)),
        ];

        let mut should_panic = false;

        for tt in tests.into_iter() {
            let evaluated = test_eval(tt.input);
            match tt.expected {
                Some(integer) =>
                    if !test_integer_object(evaluated, integer) {
                        should_panic = true
                    }
                None =>
                    if !test_null_object(evaluated) {
                        should_panic = true
                }
            }
        }

        if should_panic {
            panic!()
        }
    }

    fn test_null_object(obj: Option<object::Object>) -> bool {
        if !matches!(obj, NULL) {
            eprint!("object is not NULL. got={:?}\n", obj);
            return false
        }
        true
    }

    #[test]
    fn test_return_statements() {
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
            Test::new("return 10;", 10),
            Test::new("return 10; 9;", 10),
            Test::new("return 2 * 5; 9;", 10),
            Test::new("9; return 2 * 5; 9;", 10),
            Test::new("
                            if (10 > 1) {
                                if (10 > 1) {
                                    return 10;
                                }

                                return 1;
                            }
                            ",
                      10
            )
        ];

        let mut should_panic = false;

        for tt in tests.into_iter() {
            let evaluated = test_eval(tt.input);
            if !test_integer_object(evaluated, tt.expected) {
                should_panic = true
            }
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_error_handling() {
        struct Test {
            input: String,
            expected_message: String,
        }

        impl Test {
            fn new(input: &str, expected_message: &str) -> Self {
                Self { input: input.to_owned(), expected_message: expected_message.to_owned() }
            }
        }

        let tests = vec![
            Test::new(
                "5 + true;",
                "type mismatch: Some(IntegerObj) + Some(BooleanObj)"
            ),
            Test::new(
                "5 + true; 5;",
                "type mismatch: Some(IntegerObj) + Some(BooleanObj)"
            ),
            Test::new(
                "-true",
                "unknown operator: -Some(BooleanObj)"
            ),
            Test::new(
                "true + false;",
                "unknown operator: Some(BooleanObj) + Some(BooleanObj)"
            ),
            Test::new(
                "5; true + false; 5",
                "unknown operator: Some(BooleanObj) + Some(BooleanObj)"
            ),
            Test::new(
                "if (10 > 1) { true + false; }",
                "unknown operator: Some(BooleanObj) + Some(BooleanObj)"
            ),
            Test::new(
                "
                if (10 > 1) {
                    if (10 > 1) {
                        return true + false;
                    }

                    return 1;
                }
                ",
                "unknown operator: Some(BooleanObj) + Some(BooleanObj)"
            ),
            Test::new(
                "foobar",
                "identifier not found: foobar",
            ),
        ];

        let mut should_panic = false;

        for tt in tests.into_iter() {
            let evaluated = test_eval(tt.input);

            let err_obj = match evaluated {
                Some(object::Object::Error(inner)) => inner,
                _ => {
                    eprint!("no error object returned. got={:?}\n", evaluated);
                    should_panic = true;
                    continue
                }
            };

            if err_obj.message != tt.expected_message {
                eprint!("wrong error message. expected={}, got={}\n",
                    tt.expected_message, err_obj.message);
                should_panic = true
            }
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_let_statements() {
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
            Test::new("let a = 5; a", 5),
            Test::new("let a = 5 * 5; a", 25),
            Test::new("let a = 5; let b = a; b;", 5),
            Test::new("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        let mut should_panic = false;

        for tt in tests.into_iter() {
            if !test_integer_object(test_eval(tt.input), tt.expected) {
                should_panic = true;
            }
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";

        let evaluated = test_eval(input);

        let func = match evaluated {
            Some(Object::Function(inner)) => inner,
            _ => panic!("object is not Function. got={:?}", evaluated)
        };

        if func.parameters.len() != 1 {
            panic!("function has wrong parameters. parameters={:?}",
                func.parameters)
        }

        if func.parameters.get(0).map(|x| x.to_string()) != Some(String::from("x")) {
            panic!("parameter is not 'x'. got={:?}", func.parameters.get(0))
        }

        let expected_body = "(x + 2)";

        if func.body.to_string() != expected_body {
            panic!("body is not {}. got={}", expected_body, func.body.to_string())
        }
    }
}