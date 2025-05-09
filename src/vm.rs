use crate::{code, compiler, object};

const STACK_SIZE: usize = 2048;

pub(crate) struct VM {
    constants: Vec<object::Object>,
    instructions: code::Instructions,

    stack: [Option<object::Object>; STACK_SIZE],
    sp: usize, // Always points to the next value. Top of stack is stack[sp-1]
    last_popped_stack_elem: Option<object::Object>,
}

const TRUE: object::Object = object::Object::BooleanObj(object::BooleanObj{ value: true });
const FALSE: object::Object = object::Object::BooleanObj(object::BooleanObj{ value: false });

impl VM {
    pub(crate) fn new(bytecode: compiler::Bytecode) -> Self {
        Self {
            instructions: bytecode.instructions,
            constants: bytecode.constants,

            stack: [const { None }; STACK_SIZE],
            sp: 0,
            last_popped_stack_elem: None,
        }
    }

    pub(crate) fn stack_top(&self) -> Option<&object::Object> {
        if self.sp == 0 {
            return None
        }
        self.stack[self.sp-1].as_ref()
    }

    pub(crate) fn run(&mut self) -> Option<compiler::Error> {
        let mut ip = 0;
        while ip < self.instructions.0.len() {
            let op: code::Opcode = self.instructions.0[ip].into();

            match op {
                code::Opcode::OpConstant => {
                    let const_index = code::read_u16(&self.instructions.0[ip + 1..]);
                    ip += 2;
                    if let Some(err) = self.push(self.constants[const_index as usize].clone()) {
                        return Some(err)
                    }
                }

                code::Opcode::OpAdd | code::Opcode::OpSub | code::Opcode::OpMul | code::Opcode::OpDiv =>
                    if let Some(err) = self.execute_binary_operation(op) {
                        return Some(err)
                    }

                code::Opcode::OpEqual | code::Opcode::OpNotEqual | code::Opcode::OpGreaterThan =>
                    if let Some(err) = self.execute_comparison(op) {
                        return Some(err)
                    }

                code::Opcode::OpBang =>
                    if let Some(err) = self.execute_bang_operator() {
                        return Some(err)
                    }

                code::Opcode::OpPop => {
                    self.pop();
                }

                code::Opcode::OpTrue =>
                    if let Some(err) = self.push(TRUE) {
                        return Some(err)
                    }

                code::Opcode::OpFalse =>
                    if let Some(err) = self.push(FALSE) {
                        return Some(err)
                    }

                _ => () // TODO
            }

            ip += 1
        }

        None
    }

    fn execute_bang_operator(&mut self) -> Option<compiler::Error> {
        let operand = self.pop();

        match operand {
            Some(TRUE) => self.push(FALSE),
            Some(FALSE) => self.push(TRUE),
            _ => self.push(FALSE),
        }
    }

    fn native_bool_to_boolean_object(input: bool) -> object::Object {
        if input {
            TRUE
        } else {
            FALSE
        }
    }

    fn execute_integer_comparison(&mut self, op: code::Opcode, left: object::Object, right: object::Object) -> Option<compiler::Error> {
        let left_value = match left {
            object::Object::IntegerObj(integer_obj) => integer_obj.value,
            _ => panic!()
        };
        let right_value = match right {
            object::Object::IntegerObj(integer_obj) => integer_obj.value,
            _ => panic!()
        };

        match op {
            code::Opcode::OpEqual => self.push(Self::native_bool_to_boolean_object(right_value == left_value)),
            code::Opcode::OpNotEqual => self.push(Self::native_bool_to_boolean_object(right_value != left_value)),
            code::Opcode::OpGreaterThan => self.push(Self::native_bool_to_boolean_object(left_value > right_value)),
            _ => compiler::Error::new(format!("unknown operator: {:?}", op))
        }
    }

    fn execute_comparison(&mut self, op: code::Opcode) -> Option<compiler::Error> {
        let right = self.pop();
        let left = self.pop();

        let left_type = left.as_ref()?.inner().object_type();
        let right_type = right.as_ref()?.inner().object_type();

        if left_type == object::ObjectType::IntegerObj && right_type == object::ObjectType::IntegerObj {
            return self.execute_integer_comparison(op, left?, right?)
        }

        match op {
            code::Opcode::OpEqual => self.push(Self::native_bool_to_boolean_object(right == left)),
            code::Opcode::OpNotEqual => self.push(Self::native_bool_to_boolean_object(right != left)),
            _ => compiler::Error::new(format!(
                "unknown operator: {:?} ({:?} {:?})", op, left_type, right_type
            ))
        }
    }

    fn execute_binary_operation(&mut self, op: code::Opcode) -> Option<compiler::Error> {
        let right = self.pop();
        let left = self.pop();

        let left_type = left.as_ref()?.inner().object_type();
        let right_type = right.as_ref()?.inner().object_type();

        if left_type == object::ObjectType::IntegerObj && right_type == object::ObjectType::IntegerObj {
            return self.execute_binary_integer_operation(op, left?, right?)
        }

        compiler::Error::new(format!(
            "unsupported types for binary operation: {:?} {:?}",
            left_type, right_type
        ))
    }

    fn execute_binary_integer_operation(&mut self, op: code::Opcode, left: object::Object, right: object::Object) -> Option<compiler::Error> {
        let left_value = match left {
            object::Object::IntegerObj(integer_obj) => integer_obj.value,
            _ => panic!()
        };
        let right_value = match right {
            object::Object::IntegerObj(integer_obj) => integer_obj.value,
            _ => panic!()
        };

        let result = match op {
            code::Opcode::OpAdd => left_value + right_value,
            code::Opcode::OpSub => left_value - right_value,
            code::Opcode::OpMul => left_value * right_value,
            code::Opcode::OpDiv => left_value / right_value,
            _ => return compiler::Error::new(format!(
                "unknown integer operator: {:?}", op
            ))
        };

        self.push(object::Object::IntegerObj(object::IntegerObj { value: result }))
    }

    fn push(&mut self, o: object::Object) -> Option<compiler::Error> {
        if self.sp >= STACK_SIZE {
            return compiler::Error::new("stack overflow")
        }

        self.stack[self.sp] = Some(o);
        self.sp += 1;

        None
    }

    fn pop(&mut self) -> Option<object::Object> {
        let o = self.stack[self.sp-1].clone();
        self.sp -= 1;
        o
    }

    pub(crate) fn last_popped_stack_elem(&self) -> Option<&object::Object> {
        self.stack[self.sp].as_ref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast, compiler, lexer, object, parser};

    fn parse(input: impl Into<String>) -> ast::Program {
        let l = lexer::Lexer::new(input);
        let mut p = parser::Parser::new(l);
        p.parse_program()
    }

    fn test_integer_object(expected: i32, actual: Option<&object::Object>) -> Option<compiler::Error> {
        let result = match actual {
            Some(object::Object::IntegerObj(integer_obj)) => integer_obj,
            _ => return compiler::Error::new(format!(
                "object is not Integer. got={:?}", actual
            )),
        };

        if result.value != expected {
            return compiler::Error::new(format!(
                "object has wrong value. got={}, want={}", result.value, expected
            ))
        }

        None
    }

    struct VMTestCase {
        input: String,
        expected: Expected,
    }

    impl VMTestCase {
        fn new(str: impl Into<String>, exp: impl Into<Expected>) -> Self {
            Self {
                input: str.into(),
                expected: exp.into(),
            }
        }
    }

    fn run_vm_tests(tests: Vec<VMTestCase>) -> bool {

        let mut should_panic = false;

        for tt in tests.into_iter() {
            let program = parse(tt.input);

            let mut comp = compiler::Compiler::new();
            let err = comp.compile(ast::Node::Program(program));
            if let Some(err) = err {
                panic!("compiler error: {}", err)
            }

            let mut vm = VM::new(comp.bytecode());
            let err = vm.run();
            if let Some(err) = err {
                panic!("vm error: {}", err)
            }

            let stack_elem = vm.last_popped_stack_elem();

            if test_expected_object(tt.expected, stack_elem) {
                should_panic = true
            }
        }

        should_panic
    }

    enum Expected {
        Integer(Integer),
        Boolean(Boolean),
    }

    struct Integer(i32);
    struct Boolean(bool);

    impl Into<Expected> for i32 {
        fn into(self) -> Expected {
            Expected::Integer(Integer(self))
        }
    }

    impl Into<Expected> for bool {
        fn into(self) -> Expected {
            Expected::Boolean(Boolean(self))
        }
    }

    fn test_expected_object(expected: Expected, actual: Option<&object::Object>) -> bool {
        let mut should_panic = false;

        match expected {
            Expected::Integer(Integer(expected)) => {
                let err = test_integer_object(expected, actual);
                if let Some(err) = err {
                    should_panic = true;
                    eprintln!("test_integer_object failed: {}", err)
                }
            }

            Expected::Boolean(Boolean(expected)) => {
                let err = test_boolean_object(expected, actual);
                if let Some(err) = err {
                    should_panic = true;
                    eprintln!("test_boolean_object failed: {}", err)
                }
            }

            _ => () // TODO
        }

        should_panic
    }

    fn test_boolean_object(expected: bool, actual: Option<&object::Object>) -> Option<compiler::Error> {
        let result = match actual {
            Some(object::Object::BooleanObj(boolean_obj)) => boolean_obj,
            _ => return compiler::Error::new(format!(
                "object is not Boolean. got={:?}", actual
            ))
        };

        if result.value != expected {
            return compiler::Error::new(format!(
                "object has wrong value. got={}, want={}", result.value, expected
            ))
        };

        None
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            VMTestCase::new("1", 1),
            VMTestCase::new("2", 2),
            VMTestCase::new("1 + 2", 3),
            VMTestCase::new("1 - 2", -1),
            VMTestCase::new("1 * 2", 2),
            VMTestCase::new("4 / 2", 2),
            VMTestCase::new("50 / 2 * 2 + 10 - 5", 55),
            VMTestCase::new("5 + 5 + 5 + 5 - 10", 10),
            VMTestCase::new("2 * 2 * 2 * 2 * 2", 32),
            VMTestCase::new("5 * 2 + 10", 20),
            VMTestCase::new("5 + 2 * 10", 25),
            VMTestCase::new("5 * (2 + 10)", 60),
		    VMTestCase::new("-5", -5),
		    VMTestCase::new("-10", -10),
		    VMTestCase::new("-50 + 100 + -50", 0),
		    VMTestCase::new("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        if run_vm_tests(tests) {
            panic!()
        }
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            VMTestCase::new("true", true),
            VMTestCase::new("false", false),
		    VMTestCase::new("1 < 2", true),
		    VMTestCase::new("1 > 2", false),
		    VMTestCase::new("1 < 1", false),
		    VMTestCase::new("1 > 1", false),
		    VMTestCase::new("1 == 1", true),
		    VMTestCase::new("1 != 1", false),
		    VMTestCase::new("1 == 2", false),
		    VMTestCase::new("1 != 2", true),
		    VMTestCase::new("true == true", true),
		    VMTestCase::new("false == false", true),
		    VMTestCase::new("true == false", false),
		    VMTestCase::new("true != false", true),
		    VMTestCase::new("false != true", true),
		    VMTestCase::new("(1 < 2) == true", true),
		    VMTestCase::new("(1 < 2) == false", false),
		    VMTestCase::new("(1 > 2) == true", false),
		    VMTestCase::new("(1 > 2) == false", true),
		    VMTestCase::new("!true", false),
		    VMTestCase::new("!false", true),
		    VMTestCase::new("!5", false),
		    VMTestCase::new("!!true", true),
		    VMTestCase::new("!!false", false),
		    VMTestCase::new("!!5", true),
        ];

        if run_vm_tests(tests) {
            panic!()
        }
    }
}
