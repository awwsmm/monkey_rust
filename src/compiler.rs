use crate::{ast, code, object};

struct Error {
    message: String
}

struct Compiler {
    instructions: code::Instructions,
    constants: Vec<object::Object>,
}

impl Compiler {
    fn new() -> Self {
        Self {
            instructions: code::Instructions(vec![]),
            constants: vec![],
        }
    }

    fn compile(&self, node: ast::Node) -> Option<Error> {
        None
    }

    fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }
}

struct Bytecode {
    instructions: code::Instructions,
    constants: Vec<object::Object>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer, parser};

    fn test_instructions(
        expected: Vec<code::Instructions>,
        actual: code::Instructions,
    ) -> Option<String> {
        let concatted = concat_instructions(expected);

        if actual.0.len() != concatted.0.len() {
            return Some(format!("wrong instructions length.\nwant={}\ngot ={:?}",
                concatted, actual))
        }

        for (i, ins) in concatted.0.iter() {
            if actual.get(i) != ins {
                return Some(format!("wrong instruction at {}.\nwant={}\ngot ={:?}",
                    i, concatted, actual))
            }
        }

        None
    }

    fn parse(input: &'static str) -> ast::Program {
        let l = lexer::Lexer::new(input);
        let mut p = parser::Parser::new(l);
        p.parse_program()
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

    struct CompilerTestCase {
        input: &'static str,
        expected_constants: Vec<Expected>,
        expected_instructions: Vec<code::Instructions>,
    }

    impl CompilerTestCase {
        fn new(
            input: &'static str,
            expected_constants: Vec<Expected>,
            expected_instructions: Vec<Vec<u8>>
        ) -> CompilerTestCase {
            Self {
                input,
                expected_constants: expected_constants.into_iter().map(|e| e.into()).collect(),
                expected_instructions: expected_instructions.into_iter().map(code::Instructions).collect(),
            }
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            CompilerTestCase::new(
                "1 + 2",
                vec![1.into(), 2.into()],
                vec![
                    code::make(code::Opcode::OpConstant, vec![0]),
                ],
            )
        ];

        run_compiler_tests(tests)
    }

    fn run_compiler_tests(tests: Vec<CompilerTestCase>) {
        for tt in tests.into_iter() {
            let program = parse(tt.input);

            let compiler = Compiler::new();
            let mut err = compiler.compile(program);
            if let Some(err) = err {
                panic!("compiler error: {}", err.message)
            }

            let bytecode = compiler.bytecode();

            err = test_instructions(tt.expected_instructions, bytecode.instructions);
            if let Some(err) = err {
                panic!("test_instructions failed: {}", err.message)
            }

            err = test_constants(tt.expected_constants, bytecode.constants);
            if let Some(err) = err {
                panic!("test_constants failed: {}", err.message)
            }
        }
    }
}