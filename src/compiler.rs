use crate::{ast, code, object};
use std::fmt::{Display, Formatter};

struct Error {
    message: String
}

impl Error {
    fn new(message: String) -> Option<Error> {
        Some(Error{message})
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
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
        match node {
            ast::Node::Program(node) => {
                for s in node.statements.iter() {
                    let err = self.compile(ast::Node::Statement(s.clone()));
                    if err.is_some() {
                        return err
                    }
                }
            }

            ast::Node::Statement(ast::Statement::ExpressionStatement(node)) => {
                let err = self.compile(ast::Node::Expression(node.expression?));
                if err.is_some() {
                    return err
                }
            }

            ast::Node::Expression(ast::Expression::InfixExpression(node)) => {
                let err = self.compile(ast::Node::Expression(*node.left?));
                if err.is_some() {
                    return err
                }

                let err = self.compile(ast::Node::Expression(*node.right?));
                if err.is_some() {
                    return err
                }
            }

            ast::Node::Expression(ast::Expression::IntegerLiteral(node)) => {
                let integer = object::IntegerObj{ value: node.value };
            }

            _ => ()
        }

        None
    }

    fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }

    fn add_constant(&mut self, obj: object::Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    fn emit(&mut self, op: code::Opcode, operands: Vec<i32>) -> usize {
        let ins = code::make(op, &operands);
        self.add_instruction(ins)
    }

    fn add_instruction(&mut self, ins: Vec<u8>) -> usize {
        let pos_new_instruction = self.instructions.0.len();
        for byte in ins.iter() {
            self.instructions.0.push(*byte)
        }
        pos_new_instruction
    }
}

struct Bytecode {
    instructions: code::Instructions,
    constants: Vec<object::Object>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::Object;
    use crate::{lexer, parser};

    fn test_constants(
        expected: Vec<Expected>,
        actual: Vec<object::Object>,
    ) -> Option<Error> {
        if expected.len() != actual.len() {
            return Error::new(format!(
                "wrong number of constants. got={}, want={}", actual.len(), expected.len()
            ))
        }

        for (i, constant) in expected.into_iter().enumerate() {
            match constant {
                Expected::Integer(Integer(constant)) => {
                    let err = test_integer_object(constant, actual.get(i).cloned()?);
                    if let Some(err) = err {
                        return Error::new(format!(
                            "constant {} - test_integer_object failed: {}", i, err
                        ))
                    }
                }
            }
        }

        None
    }

    fn test_integer_object(expected: i32, actual: object::Object) -> Option<Error> {
        let result = match actual {
            Object::IntegerObj(integer_obj) => integer_obj,
            _ => return Error::new(format!(
                "object is not Integer. got={:?}", actual
            )),
        };

        if result.value != expected {
            return Error::new(format!(
                "object has wrong value. got={}, want={}", result.value, expected
            ))
        }

        None
    }

    fn test_instructions(
        expected: Vec<code::Instructions>,
        actual: code::Instructions,
    ) -> Option<Error> {
        let concatted = concat_instructions(expected);

        if actual.0.len() != concatted.0.len() {
            return Error::new(format!(
                "wrong instructions length.\nwant=\n{}\ngot =\n{}", concatted, actual
            ))
        }

        for (i, ins) in concatted.0.iter().enumerate() {
            if actual.0.get(i) != Some(ins) {
                return Error::new(format!(
                    "wrong instruction at {}.\nwant=\n{}\ngot =\n{}", i, concatted, actual
                ))
            }
        }

        None
    }

    fn concat_instructions(s: Vec<code::Instructions>) -> code::Instructions {
        let mut out = code::Instructions(vec![]);

        for ins in s.into_iter() {
            for byte in ins.0 {
                out.0.push(byte)
            }
        }

        out
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
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpConstant, &vec![1]),
                ],
            )
        ];

        run_compiler_tests(tests)
    }

    fn run_compiler_tests(tests: Vec<CompilerTestCase>) {
        for tt in tests.into_iter() {
            let program = parse(tt.input);

            let compiler = Compiler::new();
            let mut err = compiler.compile(ast::Node::Program(program));
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