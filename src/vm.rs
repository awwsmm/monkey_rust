mod frame;

use crate::object::{HasHashKey, ObjectLike};
use crate::{code, compiler, object};
use std::collections::BTreeMap;

const STACK_SIZE: usize = 2048;
pub(crate) const GLOBALS_SIZE: usize = 2048;

pub(crate) struct VM {
    constants: Vec<object::Object>,

    stack: [Option<object::Object>; STACK_SIZE],
    sp: usize, // Always points to the next value. Top of stack is stack[sp-1]

    pub(crate) globals: [Option<object::Object>; GLOBALS_SIZE],

    frames: Vec<frame::Frame>,
    frames_index: i32,
}

const TRUE: object::Object = object::Object::BooleanObj(object::BooleanObj{ value: true });
const FALSE: object::Object = object::Object::BooleanObj(object::BooleanObj{ value: false });
const NULL: object::Object = object::Object::NullObj(object::NullObj{});

const MAX_FRAMES: usize = 1024;

impl VM {
    fn current_frame(&mut self) -> &mut frame::Frame {
        &mut self.frames[(self.frames_index-1) as usize]
    }

    fn push_frame(&mut self, f: frame::Frame) {
        self.frames.push(f);
        self.frames_index += 1;
    }

    fn pop_frame(&mut self) -> frame::Frame {
        self.frames_index -= 1;
        self.frames.remove(self.frames_index as usize)
    }

    pub(crate) fn new(bytecode: compiler::Bytecode) -> Self {
        let main_func = object::CompiledFunctionObj{ instructions: bytecode.instructions };
        let main_frame = frame::Frame::new(main_func);

        let mut frames = Vec::with_capacity(MAX_FRAMES);
        frames.push(main_frame);

        Self {
            constants: bytecode.constants,

            stack: [const { None }; STACK_SIZE],
            sp: 0,

            globals: [const { None }; GLOBALS_SIZE],

            frames,
            frames_index: 1,
        }
    }

    pub(crate) fn new_with_globals_store(bytecode: compiler::Bytecode, s: [Option<object::Object>; GLOBALS_SIZE]) -> Self {
        let mut vm = Self::new(bytecode);
        vm.globals = s;
        vm
    }

    pub(crate) fn stack_top(&self) -> Option<&object::Object> {
        if self.sp == 0 {
            return None
        }
        self.stack[self.sp-1].as_ref()
    }

    pub(crate) fn run(&mut self) -> Option<compiler::Error> {
        let mut ip: usize;
        let mut ins: code::Instructions;
        let mut op: code::Opcode;

        while self.current_frame().ip < (self.current_frame().instructions().len() - 1) as i32 {
            self.current_frame().ip += 1;

            ip = self.current_frame().ip as usize;
            ins = self.current_frame().instructions();
            op = ins[ip].into();

            match op {
                code::Opcode::OpConstant => {
                    let const_index = code::read_usize(&ins[ip + 1..]);
                    self.current_frame().ip += 2;
                    if let Some(err) = self.push(self.constants[const_index].clone()) {
                        return Some(err)
                    }
                }

                code::Opcode::OpJump => {
                    let pos = code::read_usize(&ins[ip + 1..]);
                    self.current_frame().ip = (pos - 1) as i32
                }

                code::Opcode::OpJumpNotTruthy => {
                    let pos = code::read_usize(&ins[ip + 1..]);
                    self.current_frame().ip += 2;

                    let condition = self.pop();
                    if !Self::is_truthy(condition) {
                        self.current_frame().ip = (pos - 1) as i32
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

                code::Opcode::OpMinus =>
                    if let Some(err) = self.execute_minus_operator() {
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

                code::Opcode::OpNull =>
                    if let Some(err) = self.push(NULL) {
                        return Some(err)
                    }

                code::Opcode::OpSetGlobal => {
                    let global_index = code::read_usize(&ins[ip+1..]);
                    self.current_frame().ip += 2;

                    self.globals[global_index] = self.pop()
                }

                code::Opcode::OpGetGlobal => {
                    let global_index = code::read_usize(&ins[ip+1..]);
                    self.current_frame().ip += 2;

                    let obj = self.globals[global_index].clone()?;
                    if let Some(err) = self.push(obj) {
                        return Some(err)
                    }
                }

                code::Opcode::OpArray => {
                    let num_elements = code::read_usize(&ins[ip+1..]);
                    self.current_frame().ip += 2;

                    let array = self.build_array(self.sp-num_elements, self.sp);
                    self.sp -= num_elements;

                    if let Some(err) = self.push(array) {
                        return Some(err)
                    }
                }

                code::Opcode::OpHash => {
                    let num_elements = code::read_usize(&ins[ip+1..]);
                    self.current_frame().ip += 2;

                    let hash = match self.build_hash(self.sp-num_elements, self.sp) {
                        Ok(hash) => hash,
                        Err(error) => return error
                    };
                    self.sp -= num_elements;

                    if let Some(err) = self.push(hash) {
                        return Some(err)
                    }
                }

                code::Opcode::OpIndex => {
                    let index = self.pop();
                    let left = self.pop();

                    if let Some(err) = self.execute_index_expression(left, index) {
                        return Some(err)
                    }
                }

                code::Opcode::OpCall => {
                    let func = match self.stack[self.sp-1].clone() {
                        Some(object::Object::CompiledFunctionObj(obj)) => obj,
                        _ => return compiler::Error::new("calling non-function")
                    };

                    let frame = frame::Frame::new(func);
                    self.push_frame(frame)
                }

                code::Opcode::OpReturnValue => {
                    let return_value = self.pop();

                    self.pop_frame();
                    self.pop();

                    if let Some(err) = self.push(return_value?) {
                        return Some(err)
                    }
                }

                _ => () // TODO
            }
        }

        None
    }

    fn execute_hash_index(&mut self, hash: Option<object::Object>, index: Option<object::Object>) -> Option<compiler::Error> {
        let mut hash_object = match hash {
            Some(object::Object::HashObj(obj)) => obj,
            _ => panic!()
        };

        let key = match index.as_ref()?.as_hashable() {
            Some(obj) => obj,
            None => return compiler::Error::new(format!("unusable as hash key: {:?}", index.as_ref()?.object_type())),
        };

        let pair = match hash_object.pairs.remove(&key.hash_key()) {
            Some(obj) => obj,
            None => return self.push(NULL)
        };

        self.push(pair.value)
    }

    fn execute_array_index(&mut self, array: Option<object::Object>, index: Option<object::Object>) -> Option<compiler::Error> {
        let mut array_object = match array {
            Some(object::Object::ArrayObj(obj)) => obj,
            _ => panic!()
        };

        let i = match index {
            Some(object::Object::IntegerObj(obj)) => obj.value,
            _ => panic!()
        };

        let max = array_object.elements.len() as i32 - 1;

        if i < 0 || i > max {
            return self.push(NULL)
        }

        self.push(array_object.elements.remove(i as usize))
    }

    fn execute_index_expression(&mut self, left: Option<object::Object>, index: Option<object::Object>) -> Option<compiler::Error> {
        let left_type = left.as_ref()?.object_type();
        let index_type = index.as_ref()?.object_type();

        if left_type == object::ObjectType::ArrayObj && index_type == object::ObjectType::IntegerObj {
            self.execute_array_index(left, index)

        } else if left_type == object::ObjectType::HashObj {
            self.execute_hash_index(left, index)

        } else {
            compiler::Error::new(format!("index operator not supported: {:?}", left_type))
        }
    }

    fn build_hash(&self, start_index: usize, end_index: usize) -> Result<object::Object, Option<compiler::Error>> {
        let mut hashed_pairs = BTreeMap::new();

        let mut i = start_index;
        while i < end_index {
            let key = self.stack[i].as_ref().unwrap().clone();
            let value= self.stack[i+1].as_ref().unwrap().clone();

            let hash_key = match key.as_hashable() {
                Some(key) => key,
                _ => return Err(compiler::Error::new(format!("unusable as hash key: {:?}", key.object_type())))
            };

            let pair = object::HashPair{ key, value };

            hashed_pairs.insert(hash_key.hash_key(), pair);
            i += 2;
        }

        Ok(object::Object::HashObj(object::HashObj{ pairs: hashed_pairs }))
    }

    fn build_array(&self, start_index: usize, end_index: usize) -> object::Object {
        let mut elements = Vec::with_capacity(end_index - start_index);

        for i in start_index..end_index {
            elements.push(self.stack[i].as_ref().unwrap().clone());
        }

        object::Object::ArrayObj(object::ArrayObj{ elements })
    }

    fn is_truthy(obj: Option<object::Object>) -> bool {
        match obj {
            Some(object::Object::BooleanObj(obj)) => obj.value,
            Some(object::Object::NullObj(_)) => false,
            _ => true
        }
    }

    fn execute_minus_operator(&mut self) -> Option<compiler::Error> {
        let operand = self.pop();

        if operand.as_ref()?.object_type() != object::ObjectType::IntegerObj {
            return compiler::Error::new(format!(
                "unsupported type for negation: {:?}", operand.as_ref()?.object_type()
            ))
        }

        let value = match operand {
            Some(object::Object::IntegerObj(integer_obj)) => integer_obj.value,
            _ => panic!(),
        };
        self.push(object::Object::IntegerObj(object::IntegerObj{ value: -value }))
    }

    fn execute_bang_operator(&mut self) -> Option<compiler::Error> {
        let operand = self.pop();

        match operand {
            Some(TRUE) => self.push(FALSE),
            Some(FALSE) => self.push(TRUE),
            Some(NULL) => self.push(TRUE),
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
            self.execute_binary_integer_operation(op, left?, right?)

        } else if left_type == object::ObjectType::StringObj && right_type == object::ObjectType::StringObj {
            self.execute_binary_string_operation(op, left?, right?)

        } else {
            compiler::Error::new(format!(
                "unsupported types for binary operation: {:?} {:?}",
                left_type, right_type
            ))
        }
    }

    fn execute_binary_string_operation(&mut self, op: code::Opcode, left: object::Object, right: object::Object) -> Option<compiler::Error> {
        if op != code::Opcode::OpAdd {
            return compiler::Error::new(format!(
                "unknown string operator: {:?}", op
            ))
        }

        let left_value = match left {
            object::Object::StringObj(obj) => obj.value,
            _ => panic!()
        };
        let right_value = match right {
            object::Object::StringObj(obj) => obj.value,
            _ => panic!()
        };

        self.push(object::Object::StringObj(object::StringObj { value: format!("{}{}", left_value, right_value) }))
    }

    fn execute_binary_integer_operation(&mut self, op: code::Opcode, left: object::Object, right: object::Object) -> Option<compiler::Error> {
        let left_value = match left {
            object::Object::IntegerObj(obj) => obj.value,
            _ => panic!()
        };
        let right_value = match right {
            object::Object::IntegerObj(obj) => obj.value,
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
    use crate::object::HasHashKey;
    use crate::{ast, compiler, lexer, object, parser};
    use std::collections::BTreeMap;

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
        Integer(ExpectedInteger),
        Boolean(ExpectedBoolean),
        String(ExpectedString),
        IntArray(ExpectedIntArray),
        IntValueHash(ExpectedIntValueHash),
        Null,
    }

    struct ExpectedInteger(i32);
    struct ExpectedBoolean(bool);
    struct ExpectedString(&'static str);
    struct ExpectedIntArray(Vec<i32>);
    struct ExpectedIntValueHash(BTreeMap<object::HashKey, i32>);

    impl Into<Expected> for i32 {
        fn into(self) -> Expected {
            Expected::Integer(ExpectedInteger(self))
        }
    }

    impl Into<Expected> for bool {
        fn into(self) -> Expected {
            Expected::Boolean(ExpectedBoolean(self))
        }
    }

    impl Into<Expected> for &'static str {
        fn into(self) -> Expected {
            Expected::String(ExpectedString(self))
        }
    }

    impl Into<Expected> for Vec<i32> {
        fn into(self) -> Expected {
            Expected::IntArray(ExpectedIntArray(self))
        }
    }

    impl Into<Expected> for BTreeMap<object::HashKey, i32> {
        fn into(self) -> Expected {
            Expected::IntValueHash(ExpectedIntValueHash(self))
        }
    }

    fn test_expected_object(expected: Expected, actual: Option<&object::Object>) -> bool {
        let mut should_panic = false;

        match expected {
            Expected::Integer(ExpectedInteger(expected)) => {
                let err = test_integer_object(expected, actual);
                if let Some(err) = err {
                    should_panic = true;
                    eprintln!("test_integer_object failed: {}", err)
                }
            }

            Expected::Boolean(ExpectedBoolean(expected)) => {
                let err = test_boolean_object(expected, actual);
                if let Some(err) = err {
                    should_panic = true;
                    eprintln!("test_boolean_object failed: {}", err)
                }
            }

            Expected::String(ExpectedString(expected)) => {
                let err = test_string_object(expected, actual);
                if let Some(err) = err {
                    should_panic = true;
                    eprintln!("test_string_object failed: {}", err)
                }
            }

            Expected::IntArray(ExpectedIntArray(expected)) => {
                let array = match actual {
                    Some(object::Object::ArrayObj(obj)) => obj,
                    _ => {
                        should_panic = true;
                        eprintln!("object not Array: {:?}", actual);
                        return should_panic
                    }
                };

                if array.elements.len() != expected.len() {
                    should_panic = true;
                    eprintln!("wrong num of elements. want={}, got={}",
                        expected.len(), array.elements.len());
                    return should_panic
                }

                for (i, expected_elem) in expected.iter().enumerate() {
                    let err = test_integer_object(*expected_elem, array.elements.get(i));
                    if let Some(err) = err {
                        should_panic = true;
                        eprintln!("test_integer_object failed: {}", err)
                    }
                }
            }

            Expected::IntValueHash(ExpectedIntValueHash(expected)) => {
                let hash = match actual {
                    Some(object::Object::HashObj(obj)) => obj,
                    _ =>  {
                        should_panic = true;
                        eprintln!("object is not Hash. got={:?}", actual);
                        return should_panic
                    }
                };

                if hash.pairs.len() != expected.len() {
                    should_panic = true;
                    eprintln!("hash has wrong number of Pairs. want={}, got={}",
                        expected.len(), hash.pairs.len());
                    return should_panic
                }

                for (expected_key, expected_value) in expected.into_iter() {
                    let pair = match hash.pairs.get(&expected_key) {
                        Some(pair) => pair,
                        _ => {
                            should_panic = true;
                            eprintln!("no pair for given key in Pairs");
                            return should_panic
                        }
                    };

                    let err = test_integer_object(expected_value, Some(&pair.value));
                    if let Some(err) = err {
                        should_panic = true;
                        eprintln!("test_integer_object failed: {}", err)
                    }
                }
            }

            Expected::Null =>
                if actual != None {
                    eprintln!("object is not None: {:?}", actual)
                }

            _ => () // TODO
        }

        should_panic
    }

    fn test_string_object(expected: &'static str, actual: Option<&object::Object>) -> Option<compiler::Error> {
        let result = match actual {
            Some(object::Object::StringObj(obj)) => obj,
            _ => return compiler::Error::new(format!(
                "object is not String. got={:?}", actual
            ))
        };

        if result.value != expected {
            return compiler::Error::new(format!(
                "object has wrong value. got={}, want={}", result.value, expected
            ))
        };

        None
    }

    fn test_boolean_object(expected: bool, actual: Option<&object::Object>) -> Option<compiler::Error> {
        let result = match actual {
            Some(object::Object::BooleanObj(obj)) => obj,
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
            VMTestCase::new("!(if (false) { 5; })", true),
        ];

        if run_vm_tests(tests) {
            panic!()
        }
    }

    #[test]
    fn test_conditionals() {
        let tests = vec![
            VMTestCase::new("if (true) { 10 }", 10),
            VMTestCase::new("if (true) { 10 } else { 20 }", 10),
            VMTestCase::new("if (false) { 10 } else { 20 }", 20),
            VMTestCase::new("if (1) { 10 }", 10),
            VMTestCase::new("if (1 < 2) { 10 }", 10),
            VMTestCase::new("if (1 < 2) { 10 } else { 20 }", 10),
            VMTestCase::new("if (1 > 2) { 10 } else { 20 }", 20),
            VMTestCase::new("if (1 > 2) { 10 }", Expected::Null),
            VMTestCase::new("if (false) { 10 }", Expected::Null),
            VMTestCase::new("if ((if (false) { 10 })) { 10 } else { 20 }", 20),
        ];

        if run_vm_tests(tests) {
            panic!()
        }
    }

    #[test]
    fn test_global_let_statements() {
        let tests = vec![
            VMTestCase::new("let one = 1; one", 1),
            VMTestCase::new("let one = 1; let two = 2; one + two", 3),
            VMTestCase::new("let one = 1; let two = one + one; one + two", 3),
        ];

        if run_vm_tests(tests) {
            panic!()
        }
    }

    #[test]
    fn test_string_expressions() {
        let tests = vec![
            VMTestCase::new(r#""monkey"#, "monkey"),
            VMTestCase::new(r#""mon" + "key""#, "monkey"),
            VMTestCase::new(r#""mon" + "key" + "banana""#, "monkeybanana"),
        ];

        if run_vm_tests(tests) {
            panic!()
        }
    }

    #[test]
    fn test_array_literals() {
        let tests = vec![
            VMTestCase::new("[]", Vec::<i32>::new()),
            VMTestCase::new("[1, 2, 3]", vec![1, 2, 3]),
            VMTestCase::new("[1 + 2, 3 * 4, 5 + 6]", vec![3, 12, 11]),
        ];

        if run_vm_tests(tests) {
            panic!()
        }
    }

    #[test]
    fn test_hash_literals() {
        let tests = vec![
            VMTestCase::new(
                "{}",
                BTreeMap::<object::HashKey, i32>::new(),
            ),
            VMTestCase::new(
                "{1: 2, 2: 3}",
                BTreeMap::<object::HashKey, i32>::from([
                    ((object::IntegerObj{ value: 1 }).hash_key(), 2),
                    ((object::IntegerObj{ value: 2 }).hash_key(), 3),
                ]),
            ),
            VMTestCase::new(
                "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
                BTreeMap::<object::HashKey, i32>::from([
                    ((object::IntegerObj{ value: 2 }).hash_key(), 4),
                    ((object::IntegerObj{ value: 6 }).hash_key(), 16),
                ]),
            ),
        ];

        if run_vm_tests(tests) {
            panic!()
        }
    }

    #[test]
    fn test_index_expressions() {
        let tests = vec![
            VMTestCase::new("[1, 2, 3][1]", 2),
            VMTestCase::new("[1, 2, 3][0 + 2]", 3),
            VMTestCase::new("[[1, 1, 1]][0][0]", 1),
            VMTestCase::new("[][0]", Expected::Null),
            VMTestCase::new("[1, 2, 3][99]", Expected::Null),
            VMTestCase::new("[1][-1]", Expected::Null),
            VMTestCase::new("{1: 1, 2: 2}[1]", 1),
            VMTestCase::new("{1: 1, 2: 2}[2]", 2),
            VMTestCase::new("{1: 1}[0]", Expected::Null),
            VMTestCase::new("{}[0]", Expected::Null),
        ];

        if run_vm_tests(tests) {
            panic!()
        }
    }

    #[test]
    fn test_calling_functions_without_arguments() {
        let tests = vec![
            VMTestCase::new(
                r#"
                let fivePlusTen = fn() { 5 + 10; };
                fivePlusTen();
                "#,
                15,
            ),
            VMTestCase::new(
                r#"
                let one = fn() { 1; };
                let two = fn() { 2; };
                one() + two()
                "#,
                3,
            ),
            VMTestCase::new(
                r#"
                let a = fn() { 1 };
                let b = fn() { a() + 1 };
                let c = fn() { b() + 1 };
                c();
                "#,
                3,
            ),
        ];

        if run_vm_tests(tests) {
            panic!()
        }
    }

    #[test]
    fn test_functions_with_return_statement() {
        let tests = vec![
            VMTestCase::new(
                r#"
                let earlyExit = fn() { return 99; 100; };
			    earlyExit();
                "#,
                99,
            ),
            VMTestCase::new(
                r#"
                let earlyExit = fn() { return 99; return 100; };
			    earlyExit();
                "#,
                99,
            ),
        ];

        if run_vm_tests(tests) {
            panic!()
        }
    }
}
