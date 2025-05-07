use crate::{code, compiler, object};

const STACK_SIZE: usize = 2048;

struct VM {
    constants: Vec<object::Object>,
    instructions: code::Instructions,

    stack: [Option<Box<object::Object>>; STACK_SIZE],
    sp: usize, // Always points to the next value. Top of stack is stack[sp-1
}

impl VM {
    fn new(bytecode: compiler::Bytecode) -> Self {
        Self {
            instructions: bytecode.instructions,
            constants: bytecode.constants,

            stack: [const { None }; STACK_SIZE],
            sp: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast, compiler, lexer, object, parser};

    fn parse(input: &'static str) -> ast::Program {
        let l = lexer::Lexer::new(input);
        let mut p = parser::Parser::new(l);
        p.parse_program()
    }

    fn test_integer_object(expected: i32, actual: object::Object) -> Option<compiler::Error> {
        let result = match actual {
            object::Object::IntegerObj(integer_obj) => integer_obj,
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

    fn run_vm_tests(tests: Vec<VMTestCase>) {
        for tt in tests.into_iter() {
            let program = parse(&tt.input);

            let mut comp = compiler::Compiler::new();
            let err = comp.compile(ast::Node::Program(program));
            if let Some(err) = err {
                panic!("compiler error: {}", err)
            }

            let vm = new(comp.bytecode());
            let err = vm.run();
            if let Some(err) = err {
                panic!("vm error: {}", err)
            }

            let stack_elem = vm.stack_top();

            test_expected_object(tt.expected, stack_elem)
        }
    }

    enum Expected {
        Integer(Integer),
    }

    struct Integer(i32);

    impl Into<Expected> for i32 {
        fn into(self) -> Expected {
            Expected::Integer(Integer(self))
        }
    }

    fn test_expected_object(expected: Expected, actual: object::Object) -> bool {
        let mut should_panic = false;

        match expected {
            Expected::Integer(Integer(expected)) => {
                let err = test_integer_object(expected, actual);
                if let Some(err) = err {
                    should_panic = true;
                    eprintln!("test_integer_object failed: {}", err)
                }
            }
        }

        should_panic
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            VMTestCase::new("1", 1),
            VMTestCase::new("2", 2),
            VMTestCase::new("1 + 2", 2), // FIXME
        ];

        run_vm_tests(tests)
    }
}
