mod builtins;

use crate::object::{IsError, ObjectLike};
use crate::{ast, object};
use std::cell::RefCell;
use std::rc::Rc;

const NULL: object::NullObj = object::NullObj {};
const TRUE: object::BooleanObj = object::BooleanObj { value: true };
const FALSE: object::BooleanObj = object::BooleanObj { value: false };

pub(crate) fn eval(node: Option<ast::Node>, env: Rc<RefCell<object::environment::Environment>>) -> Option<object::Object> {
    match node {

        // Statements
        Some(ast::Node::Program(node)) =>
            eval_program(node, Rc::clone(&env)),

        Some(ast::Node::Statement(ast::Statement::ExpressionStatement(node))) =>
            eval(node.expression.map(ast::Node::Expression), Rc::clone(&env)),

        Some(ast::Node::Statement(ast::Statement::BlockStatement(node))) =>
            eval_block_statement(node, Rc::clone(&env)),

        Some(ast::Node::Statement(ast::Statement::ReturnStatement(node))) => {
            let val = eval(node.return_value.map(ast::Node::Expression), Rc::clone(&env));
            if val.is_error() {
                return val
            }
            Some(object::Object::ReturnValueObj(object::ReturnValueObj { value: val.map(Box::new) }))
        }

        Some(ast::Node::Statement(ast::Statement::LetStatement(node))) => {
            let val = eval(node.value.map(ast::Node::Expression), Rc::clone(&env));
            if val.is_error() {
                return val
            }
            env.borrow_mut().set(node.name?.value.as_str(), val?);
            None
        }

        // Expressions
        Some(ast::Node::Expression(ast::Expression::PrefixExpression(node))) => {
            let right = eval(node.right.map(|e| ast::Node::Expression(*e)), Rc::clone(&env));
            if right.is_error() {
                return right
            }
            eval_prefix_expression(node.operator.as_str(), right)
        }

        Some(ast::Node::Expression(ast::Expression::InfixExpression(node))) => {
            let left = eval(node.left.map(|e| ast::Node::Expression(*e)), Rc::clone(&env));
            if left.is_error() {
                return left
            }

            let right = eval(node.right.map(|e| ast::Node::Expression(*e)), Rc::clone(&env));
            if right.is_error() {
                return right
            }

            eval_infix_expression(node.operator.as_str(), left, right)
        }

        Some(ast::Node::Expression(ast::Expression::IfExpression(node))) =>
            eval_if_expression(node, Rc::clone(&env)),

        Some(ast::Node::Expression(ast::Expression::IntegerLiteral(node))) =>
            Some(object::Object::IntegerObj(object::IntegerObj { value: node.value })),

        Some(ast::Node::Expression(ast::Expression::Boolean(node))) =>
            native_bool_to_boolean_object(node.value),

        Some(ast::Node::Expression(ast::Expression::Identifier(node))) =>
            eval_identifier(node, Rc::clone(&env)),

        Some(ast::Node::Expression(ast::Expression::FunctionLiteral(node))) => {
            let params = node.parameters;
            let body = node.body;
            Some(object::Object::FunctionObj(object::FunctionObj {
                parameters: params,
                body: body?,
                env: Rc::clone(&env),
            }))
        }

        Some(ast::Node::Expression(ast::Expression::CallExpression(node))) => {
            let function = eval(Some(ast::Node::Expression(*node.function)), Rc::clone(&env));
            if function.is_error() {
                return function
            }
            let args = eval_expressions(node.arguments, Rc::clone(&env));

            if args.len() == 1 {
                let args_0 = args.get(0).cloned();
                if args_0.is_error() {
                    return args_0
                }
            }

            apply_function(function?, args)
        }

        Some(ast::Node::Expression(ast::Expression::StringLiteral(node))) =>
            Some(object::Object::StringObj(object::StringObj{ value: node.value })),

        Some(ast::Node::Expression(ast::Expression::ArrayLiteral(node))) => {
            let elements = eval_expressions(node.elements, Rc::clone(&env));
            let elements_0 = elements.get(0).cloned();
            if elements.len() == 1 && elements_0.is_error() {
                return elements_0
            }
            Some(object::Object::ArrayObj(object::ArrayObj{ elements }))
        }

        Some(ast::Node::Expression(ast::Expression::IndexExpression(node))) => {
            let left = eval(Some(ast::Node::Expression(*node.left)), Rc::clone(&env));
            if left.is_error() {
                return left
            }
            let index = eval(Some(ast::Node::Expression(*node.index?)), Rc::clone(&env));
            if index.is_error() {
                return index
            }
            eval_index_expression(left, index)
        }

        _ => None
    }
}

fn eval_index_expression(left: Option<object::Object>, index: Option<object::Object>) -> Option<object::Object> {
    let left_type = left.as_ref()?.object_type();
    let index_type = index.as_ref()?.object_type();

    if left_type == object::ObjectType::ArrayObj && index_type == object::ObjectType::IntegerObj {
        eval_array_index_expression(left, index)
    } else {
        object::ErrorObj::new(format!("index operator not supported: {:?}", left_type))
    }
}

fn eval_array_index_expression(array: Option<object::Object>, index: Option<object::Object>) -> Option<object::Object> {
    let array_object = match array {
        Some(object::Object::ArrayObj(inner)) => inner,
        _ => panic!()
    };

    let idx = match index {
        Some(object::Object::IntegerObj(inner)) => inner.value as usize,
        _ => panic!()
    };

    let max = array_object.elements.len() - 1;

    if idx > max {
        return Some(object::Object::NullObj(NULL))
    }

    array_object.elements.get(idx).cloned()
}

fn apply_function(func: object::Object, args: Vec<object::Object>) -> Option<object::Object> {
    match func {
        object::Object::FunctionObj(func) => {
            let extended_env = extend_function_env(func.clone(), args);
            let evaluated = eval(
                Some(ast::Node::Statement(ast::Statement::BlockStatement(func.body))),
                Rc::new(RefCell::new(extended_env))
            );
            unwrap_return_value(evaluated?).map(|x| *x)
        },

        object::Object::BuiltinObj(func) =>
            Some((func.func)(args)),

        _ =>
            object::ErrorObj::new(format!("not a function: {:?}", func.object_type()))
    }
}

fn extend_function_env(func: object::FunctionObj, args: Vec<object::Object>) -> object::environment::Environment {
    let mut env = object::environment::Environment::new(Some(Rc::clone(&func.env)));

    for (param_idx, param) in func.parameters.iter().enumerate() {
        env.set(param.value.as_str(), args.get(param_idx).cloned().unwrap());
    }

    env
}

fn unwrap_return_value(obj: object::Object) -> Option<Box<object::Object>> {
    if let object::Object::ReturnValueObj(return_value) = obj {
        return return_value.value
    }

    Some(Box::new(obj))
}

fn eval_expressions(exps: Vec<ast::Expression>, env: Rc<RefCell<object::environment::Environment>>) -> Vec<object::Object> {
    let mut result = vec![];

    for e in exps.into_iter() {
        let evaluated = eval(Some(ast::Node::Expression(e)), Rc::clone(&env));
        if evaluated.is_error() {
            return vec![evaluated].into_iter().flatten().collect()
        }
        result.push(evaluated)
    }

    result.into_iter().flatten().collect()
}

fn eval_identifier(node: ast::Identifier, env: Rc<RefCell<object::environment::Environment>>) -> Option<object::Object> {
    if let Some(val) = env.borrow().get(node.value.as_str()) {
        return Some(val)
    }

    if let Some(builtin) = builtins::Builtin::from(node.value.as_str()) {
        return Some(object::Object::BuiltinObj(builtin))
    }

    object::ErrorObj::new(format!("identifier not found: {}", node.value))
}

fn eval_program(program: ast::Program, env: Rc<RefCell<object::environment::Environment>>) -> Option<object::Object> {
    let mut result: Option<object::Object> = None;

    for statement in program.statements.into_iter() {
        result = eval(Some(ast::Node::Statement(statement)), Rc::clone(&env));

        if let Some(object::Object::ReturnValueObj(return_value)) = result {
            return return_value.value.map(|x| *x)

        } else if matches!(result, Some(object::Object::ErrorObj(_))) {
            return result
        }
    }

    result
}

fn eval_if_expression(ie: ast::IfExpression, env: Rc<RefCell<object::environment::Environment>>) -> Option<object::Object> {
    let condition = eval(ie.condition.map(|e| ast::Node::Expression(*e)) ,Rc::clone(&env));
    if condition.is_error() {
        return condition
    }

    if is_truthy(condition) {
        eval(ie.consequence.map(|x| ast::Node::Statement(ast::Statement::BlockStatement(x))), Rc::clone(&env))
    } else if ie.alternative.is_some() {
        eval(ie.alternative.map(|x| ast::Node::Statement(ast::Statement::BlockStatement(x))), Rc::clone(&env))
    } else {
        Some(object::Object::NullObj(NULL))
    }
}

fn is_truthy(obj: Option<object::Object>) -> bool {
    match obj {
        Some(object::Object::NullObj(NULL)) => false,
        Some(object::Object::BooleanObj(TRUE)) => true,
        Some(object::Object::BooleanObj(FALSE)) => false,
        _ => true,
    }
}

fn native_bool_to_boolean_object(input: bool) -> Option<object::Object> {
    if input {
        return Some(object::Object::BooleanObj(TRUE))
    }
    Some(object::Object::BooleanObj(FALSE))
}

fn eval_block_statement(block: ast::BlockStatement, env: Rc<RefCell<object::environment::Environment>>) -> Option<object::Object> {
    let mut result: Option<object::Object> = None;

    for statement in block.statements.into_iter() {
        result = eval(Some(ast::Node::Statement(statement)), Rc::clone(&env));

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
        _ => object::ErrorObj::new(format!("unknown operator: {}{:?}", operator, right.as_ref().map(|x| x.object_type())))
    }
}

fn eval_bang_operator_expression(right: Option<object::Object>) -> Option<object::Object> {
    match right {
        Some(object::Object::BooleanObj(TRUE)) => Some(object::Object::BooleanObj(FALSE)),
        Some(object::Object::BooleanObj(FALSE)) => Some(object::Object::BooleanObj(TRUE)),
        Some(object::Object::NullObj(NULL)) => Some(object::Object::BooleanObj(TRUE)),
        _ => Some(object::Object::BooleanObj(FALSE)),
    }
}

fn eval_minus_prefix_operator_expression(right: Option<object::Object>) -> Option<object::Object> {
    let right_type = right.as_ref().map(|x| x.object_type());
    if right_type != Some(object::ObjectType::IntegerObj) {
        return object::ErrorObj::new(format!("unknown operator: -{:?}", right_type))
    }

    let value = match right? {
        object::Object::IntegerObj(inner) => inner.value,
        _ => panic!()
    };
    Some(object::Object::IntegerObj(object::IntegerObj { value: -value }))
}

fn eval_infix_expression(operator: &str, left: Option<object::Object>, right: Option<object::Object>) -> Option<object::Object> {
    let left_type = left.as_ref().map(|x| x.object_type());
    let right_type = right.as_ref().map(|x| x.object_type());

    if left_type == Some(object::ObjectType::IntegerObj) && right_type == Some(object::ObjectType::IntegerObj) {
        eval_integer_infix_expression(operator, left, right)

    } else if left_type == Some(object::ObjectType::StringObj) && right_type == Some(object::ObjectType::StringObj) {
        eval_string_infix_expression(operator, left, right)

    } else if operator == "==" {
        native_bool_to_boolean_object(left == right)

    } else if operator == "!=" {
        native_bool_to_boolean_object(left != right)

    } else if left_type != right_type {
        object::ErrorObj::new(format!("type mismatch: {:?} {} {:?}", left_type, operator, right_type))

    } else {
        object::ErrorObj::new(format!("unknown operator: {:?} {} {:?}", left_type, operator, right_type))
    }
}

fn eval_string_infix_expression(operator: &str, left: Option<object::Object>, right: Option<object::Object>) -> Option<object::Object> {
    let left_type = left.as_ref().map(|x| x.object_type());
    let right_type = right.as_ref().map(|x| x.object_type());

    if operator != "+" {
        return object::ErrorObj::new(format!("unknown operator: {:?} {} {:?}", left_type, operator, right_type))
    }

    let left_val = match left {
        Some(object::Object::StringObj(inner)) => inner.value,
        _ => panic!()
    };
    let right_val = match right {
        Some(object::Object::StringObj(inner)) => inner.value,
        _ => panic!()
    };

    Some(object::Object::StringObj(object::StringObj{ value: format!("{}{}", left_val, right_val) }))
}

fn eval_integer_infix_expression(operator: &str, left: Option<object::Object>, right: Option<object::Object>) -> Option<object::Object> {
    let left_type = left.as_ref().map(|x| x.object_type());
    let right_type = right.as_ref().map(|x| x.object_type());

    let left_val = match left {
        Some(object::Object::IntegerObj(inner)) => inner.value,
        _ => panic!()
    };
    let right_val = match right {
        Some(object::Object::IntegerObj(inner)) => inner.value,
        _ => panic!()
    };

    match operator {
        "+" => Some(object::Object::IntegerObj(object::IntegerObj { value: left_val + right_val })),
        "-" => Some(object::Object::IntegerObj(object::IntegerObj { value: left_val - right_val })),
        "*" => Some(object::Object::IntegerObj(object::IntegerObj { value: left_val * right_val })),
        "/" => Some(object::Object::IntegerObj(object::IntegerObj { value: left_val / right_val })),
        "<" => native_bool_to_boolean_object(left_val < right_val),
        ">" => native_bool_to_boolean_object(left_val > right_val),
        "==" => native_bool_to_boolean_object(left_val == right_val),
        "!=" => native_bool_to_boolean_object(left_val != right_val),
        _ => object::ErrorObj::new(format!("unknown operator: {:?} {} {:?}", left_type, operator, right_type))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::{HasHashKey, Hashable};
    use crate::{lexer, object, parser};
    use std::collections::BTreeMap;

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
        let env = object::environment::Environment::new(None);

        eval(Some(ast::Node::Program(program)), Rc::new(RefCell::new(env)))
    }

    fn test_integer_object(obj: Option<object::Object>, expected: i32) -> bool {
        let result = match obj {
            Some(object::Object::IntegerObj(inner)) => inner,
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
            Some(object::Object::BooleanObj(inner)) => inner,
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
        if !matches!(obj, Some(object::Object::NullObj(NULL))) {
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
            Test::new(
                r#""Hello" - "World""#,
                "unknown operator: Some(StringObj) - Some(StringObj)"
            )
        ];

        let mut should_panic = false;

        for tt in tests.into_iter() {
            let evaluated = test_eval(tt.input);

            let err_obj = match evaluated {
                Some(object::Object::ErrorObj(inner)) => inner,
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
            Some(object::Object::FunctionObj(inner)) => inner,
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

    #[test]
    fn test_function_application() {
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
		    Test::new("let identity = fn(x) { x; }; identity(5);", 5),
		    Test::new("let identity = fn(x) { return x; }; identity(5);", 5),
		    Test::new("let double = fn(x) { x * 2; }; double(5);", 10),
		    Test::new("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
		    Test::new("let add = fn(x, y) { x + y; } add(5 + 5, add(5, 5));", 20),
		    Test::new("fn(x) { x; }(5)", 5),
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
    fn test_closures() {
        let input ="
            let newAdder = fn(x) {
                fn(y) { x + y };
            };

            let addTwo = newAdder(2);
            addTwo(2);
        ";

        if !test_integer_object(test_eval(input), 4) {
            panic!()
        }
    }

    #[test]
    fn test_string_literal() {
        let input = r#""Hello World!"#;

        let evaluated = test_eval(input);
        let str = match evaluated {
            Some(object::Object::StringObj(str)) => str,
            _ => panic!("object is not String. got={:?}", evaluated)
        };

        if str.value != "Hello World!" {
            panic!("String has wrong value. got={}", str.value)
        }
    }

    #[test]
    fn test_string_concatenation() {
        let input = r#""Hello" + " " + "World!"#;

        let evaluated = test_eval(input);
        let str = match evaluated {
            Some(object::Object::StringObj(str)) => str,
            _ => panic!("object is not String. got={:?}", evaluated)
        };

        if str.value != "Hello World!" {
            panic!("String has wrong value. got={}", str.value)
        }
    }


    enum Expected {
        Integer(i32),
        String(String),
    }

    impl From<i32> for Expected {
        fn from(value: i32) -> Self {
            Self::Integer(value)
        }
    }

    impl From<&str> for Expected {
        fn from(value: &str) -> Self {
            Self::String(value.to_owned())
        }
    }

    #[test]
    fn test_builtin_functions() {
        struct Test {
            input: String,
            expected: Expected,
        }

        impl Test {
            fn new(input: &str, expected: impl Into<Expected>) -> Self {
                Self {
                    input: input.to_owned(),
                    expected: expected.into(),
                }
            }
        }

        let tests = vec![
            Test::new(r#"len("")"#, 0),
            Test::new(r#"len("four")"#, 4),
            Test::new(r#"len("hello world")"#, 11),
            Test::new(r#"len(1)"#, "argument to `len` not supported, got Some(IntegerObj)"),
            Test::new(r#"len("one", "two")"#, "wrong number of arguments. got=2, want=1"),
        ];

        let mut should_panic = false;

        for tt in tests.into_iter() {
            let evaluated = test_eval(tt.input);

            match tt.expected {
                Expected::Integer(expected) =>
                    if !test_integer_object(evaluated, expected) {
                        should_panic = true
                    }

                Expected::String(expected) => {

                    let err_obj = match evaluated {
                        Some(object::Object::ErrorObj(err_obj)) => err_obj,
                        _ => {
                            eprint!("object is not Error. got={:?}", evaluated);
                            should_panic = true;
                            continue
                        }
                    };

                    if err_obj.message != expected {
                        eprint!("wrong error message. expected={}, got={}",
                            expected, err_obj.message);
                        should_panic = true;
                    }
                }
            }
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";

        let evaluated = test_eval(input);
        let result = match evaluated {
            Some(object::Object::ArrayObj(inner)) => inner,
            _ => panic!("object is not Array. got={:?}", evaluated)
        };

        if result.elements.len() != 3 {
            panic!("array has wrong num of elements. got={}",
                result.elements.len())
        }

        let mut should_panic = false;

        if !test_integer_object(result.elements.get(0).cloned(), 1) {
            should_panic = true
        }

        if !test_integer_object(result.elements.get(1).cloned(), 4) {
            should_panic = true
        }

        if !test_integer_object(result.elements.get(2).cloned(), 6) {
            should_panic = true
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_array_index_expressions() {
        struct Test {
            input: String,
            expected: Option<i32>,
        }

        impl Test {
            fn new(input: &str, expected: Option<i32>) -> Self {
                Self {
                    input: input.to_owned(),
                    expected,
                }
            }
        }

        let tests = vec![
            Test::new(
                "[1, 2, 3][0]",
                Some(1)
            ),
            Test::new(
                "[1, 2, 3][1]",
                Some(2)
            ),
            Test::new(
                "[1, 2, 3][2]",
                Some(3)
            ),
            Test::new(
                "let i = 0; [1][i]",
                Some(1)
            ),
            Test::new(
                "[1, 2, 3][1 + 1]",
                Some(3)
            ),
            Test::new(
                "let myArray = [1, 2, 3]; myArray[2];",
                Some(3)
            ),
            Test::new(
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Some(6)
            ),
            Test::new(
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Some(2)
            ),
            Test::new(
                "[1, 2, 3][3]",
                None
            ),
            Test::new(
                "[1, 2, 3][-1]",
                None
            )
        ];

        let mut should_panic = false;

        for tt in tests.into_iter() {
            let evaluated = test_eval(tt.input);
            let result = match tt.expected {
                Some(integer) => test_integer_object(evaluated, integer),
                None => test_null_object(evaluated),
            };

            if !result {
                should_panic = true
            }
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_hash_literals() {
        let input = r#"let two = "two";
        {
            "one": 10 - 9,
            two: 1 + 1,
            "thr" + "ee": 6 / 2,
            4: 4,
            true: 5,
            false: 6
        }"#;

        let evaluated = test_eval(input);
        let result = match evaluated {
            Some(object::Object::HashObj(inner)) => inner,
            _ => panic!("Eval didn't return Hash. got={:?}", evaluated)
        };

        let mut expected = BTreeMap::new();
        expected.insert(Hashable::StringObj(object::StringObj{ value: "one".to_string() }).hash_key(), 1);
        expected.insert(Hashable::StringObj(object::StringObj{ value: "two".to_string() }).hash_key(), 2);
        expected.insert(Hashable::StringObj(object::StringObj{ value: "three".to_string() }).hash_key(), 3);
        expected.insert(Hashable::IntegerObj(object::IntegerObj{ value: 4 }).hash_key(), 4);
        expected.insert(Hashable::BooleanObj(TRUE).hash_key(), 5);
        expected.insert(Hashable::BooleanObj(FALSE).hash_key(), 6);

        if result.pairs.len() != expected.len() {
            panic!("Hash has wrong num of pairs. got={}", result.pairs.len())
        }

        let mut should_panic = false;

        for (expected_key, expected_value) in expected.iter() {
            let pair = match result.pairs.get(expected_key) {
                Some(value) => value,
                None => {
                    should_panic = true;
                    eprintln!("no pair for given key in pairs");
                    continue
                }
            };

            if !test_integer_object(Some(pair.value.clone()), *expected_value) {
                should_panic = true
            }
        }

    }
}