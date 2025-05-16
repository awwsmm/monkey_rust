use crate::{ast, code, object};
use std::cmp::PartialEq;
use std::fmt::{Display, Formatter};

pub(crate) struct Error {
    message: String
}

impl Error {
    pub(crate) fn new(message: impl Into<String>) -> Option<Error> {
        Some(Error{message: message.into()})
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

#[derive(Clone, Copy)]
struct EmittedInstruction {
    opcode: Option<code::Opcode>,
    position: usize,
}

pub(crate) struct Compiler {
    instructions: code::Instructions,
    constants: Vec<object::Object>,

    last_instruction: EmittedInstruction,
    previous_instruction: EmittedInstruction,
}

impl Compiler {
    pub(crate) fn new() -> Self {
        Self {
            instructions: code::Instructions(vec![]),
            constants: vec![],

            last_instruction: EmittedInstruction{ opcode: None, position: 0 },
            previous_instruction: EmittedInstruction{ opcode: None, position: 0 },
        }
    }

    pub(crate) fn compile(&mut self, node: ast::Node) -> Option<Error> {
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
                self.emit(code::Opcode::OpPop, vec![]);
            }

            ast::Node::Statement(ast::Statement::BlockStatement(node)) => {
                for s in node.statements.into_iter() {
                    let err = self.compile(ast::Node::Statement(s));
                    if err.is_some() {
                        return err
                    }
                }
            }

            ast::Node::Expression(ast::Expression::IfExpression(node)) => {
                let err = self.compile(ast::Node::Expression(*node.condition?));
                if err.is_some() {
                    return err
                }

                let jump_not_truthy_pos = self.emit(code::Opcode::OpJumpNotTruthy, vec![9999]);

                let err = self.compile(ast::Node::Statement(ast::Statement::BlockStatement(node.consequence?)));
                if err.is_some() {
                    return err
                }

                if self.last_instruction_is_pop() {
                    self.remove_last_pop()
                }

                if node.alternative.is_none() {
                    let after_consequence_pos = self.instructions.len();
                    self.change_operand(jump_not_truthy_pos, after_consequence_pos)
                } else {
                    // Emit an `OpJump` with a bogus value
                    self.emit(code::Opcode::OpJump, vec![9999]);

                    let after_consequence_pos = self.instructions.len();
                    self.change_operand(jump_not_truthy_pos, after_consequence_pos)
                }
            }

            ast::Node::Expression(ast::Expression::PrefixExpression(node)) => {
                let err = self.compile(ast::Node::Expression(*node.right?));
                if err.is_some() {
                    return err
                }

                match node.operator.as_str() {
                    "!" => self.emit(code::Opcode::OpBang, vec![]),
                    "-" => self.emit(code::Opcode::OpMinus, vec![]),
                    _ => return Error::new(format!("unknown operator {}", node.operator)),
                };
            }

            ast::Node::Expression(ast::Expression::InfixExpression(node)) => {
                if node.operator == "<" {
                    let err = self.compile(ast::Node::Expression(*node.right?));
                    if err.is_some() {
                        return err
                    }

                    let err = self.compile(ast::Node::Expression(*node.left?));
                    if err.is_some() {
                        return err
                    }

                    self.emit(code::Opcode::OpGreaterThan, vec![]);
                    return None
                }

                let err = self.compile(ast::Node::Expression(*node.left?));
                if err.is_some() {
                    return err
                }

                let err = self.compile(ast::Node::Expression(*node.right?));
                if err.is_some() {
                    return err
                }

                match node.operator.as_str() {
                    "+" => self.emit(code::Opcode::OpAdd, vec![]),
                    "-" => self.emit(code::Opcode::OpSub, vec![]),
                    "*" => self.emit(code::Opcode::OpMul, vec![]),
                    "/" => self.emit(code::Opcode::OpDiv, vec![]),
                    ">" => self.emit(code::Opcode::OpGreaterThan, vec![]),
                    "==" => self.emit(code::Opcode::OpEqual, vec![]),
                    "!=" => self.emit(code::Opcode::OpNotEqual, vec![]),
                    _ => return Error::new(format!("unknown operator {}", node.operator))
                };
            }

            ast::Node::Expression(ast::Expression::IntegerLiteral(node)) => {
                let integer = object::Object::IntegerObj(object::IntegerObj{ value: node.value });
                let constant = self.add_constant(integer);
                self.emit(code::Opcode::OpConstant, vec![constant]);
            }

            ast::Node::Expression(ast::Expression::Boolean(node)) => {
                if node.value {
                    self.emit(code::Opcode::OpTrue, vec![])
                } else {
                    self.emit(code::Opcode::OpFalse, vec![])
                };
            }

            _ => ()
        }

        None
    }

    fn last_instruction_is_pop(&mut self) -> bool {
        self.last_instruction.opcode == Some(code::Opcode::OpPop)
    }

    fn remove_last_pop(&mut self) {
        self.instructions.truncate(self.last_instruction.position);
        self.last_instruction = self.previous_instruction;
    }

    pub(crate) fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }

    fn add_constant(&mut self, obj: object::Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    fn emit(&mut self, op: code::Opcode, operands: Vec<usize>) -> usize {
        let ins = code::make(op, &operands);
        let pos = self.add_instruction(ins);

        self.set_last_instruction(op, pos);

        pos
    }

    fn set_last_instruction(&mut self, op: code::Opcode, pos: usize) {
        let previous = self.last_instruction;
        let last = EmittedInstruction{ opcode: Some(op), position: pos };

        self.previous_instruction = previous;
        self.last_instruction = last;
    }

    fn add_instruction(&mut self, ins: Vec<u8>) -> usize {
        let pos_new_instruction = self.instructions.len();
        for byte in ins.iter() {
            self.instructions.push(*byte)
        }
        pos_new_instruction
    }

    fn replace_instructions(&mut self, pos: usize, new_instruction: Vec<u8>) {
        for (i, byte) in new_instruction.iter().enumerate() {
            self.instructions[pos+i] = *byte;
        }
    }

    fn change_operand(&mut self, op_pos: usize, operand: usize) {
        let op: code::Opcode = self.instructions[op_pos].into();
        let new_instruction = code::make(op, &vec![operand]);

        self.replace_instructions(op_pos, new_instruction)
    }
}

pub(crate) struct Bytecode {
    pub(crate) instructions: code::Instructions,
    pub(crate) constants: Vec<object::Object>,
}

#[cfg(test)]
mod tests {
    use super::*;
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

                _ => () // TODO
            }
        }

        None
    }

    fn test_integer_object(expected: i32, actual: object::Object) -> Option<Error> {
        let result = match actual {
            object::Object::IntegerObj(integer_obj) => integer_obj,
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

        if actual.len() != concatted.len() {
            return Error::new(format!(
                "wrong instructions length.\nwant=\n{}\ngot =\n{}", concatted, actual
            ))
        }

        for (i, ins) in concatted.iter().enumerate() {
            if actual.get(i) != Some(ins) {
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
                out.push(byte)
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
                    code::make(code::Opcode::OpAdd, &vec![]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "1; 2",
                vec![1.into(), 2.into()],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpPop, &vec![]),
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "1 - 2",
                vec![1.into(), 2.into()],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    code::make(code::Opcode::OpSub, &vec![]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "1 * 2",
                vec![1.into(), 2.into()],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    code::make(code::Opcode::OpMul, &vec![]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "2 / 1",
                vec![2.into(), 1.into()],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    code::make(code::Opcode::OpDiv, &vec![]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "-1",
                vec![1.into()],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpMinus, &vec![]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            CompilerTestCase::new(
                "true",
                vec![],
                vec![
                    code::make(code::Opcode::OpTrue, &vec![]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "false",
                vec![],
                vec![
                    code::make(code::Opcode::OpFalse, &vec![]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "1 > 2",
                vec![1.into(), 2.into()],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    code::make(code::Opcode::OpGreaterThan, &vec![]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "1 < 2",
                vec![2.into(), 1.into()],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    code::make(code::Opcode::OpGreaterThan, &vec![]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "1 == 2",
                vec![1.into(), 2.into()],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    code::make(code::Opcode::OpEqual, &vec![]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "1 != 2",
                vec![1.into(), 2.into()],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    code::make(code::Opcode::OpNotEqual, &vec![]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "true == false",
                vec![],
                vec![
                    code::make(code::Opcode::OpTrue, &vec![]),
                    code::make(code::Opcode::OpFalse, &vec![]),
                    code::make(code::Opcode::OpEqual, &vec![]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "true != false",
                vec![],
                vec![
                    code::make(code::Opcode::OpTrue, &vec![]),
                    code::make(code::Opcode::OpFalse, &vec![]),
                    code::make(code::Opcode::OpNotEqual, &vec![]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "!true",
                vec![],
                vec![
                    code::make(code::Opcode::OpTrue, &vec![]),
                    code::make(code::Opcode::OpBang, &vec![]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
        ];

        run_compiler_tests(tests)
    }

    fn run_compiler_tests(tests: Vec<CompilerTestCase>) {
        for tt in tests.into_iter() {
            let program = parse(tt.input);

            let mut compiler = Compiler::new();
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

    #[test]
    fn test_comditionals() {
        let tests = vec![
            CompilerTestCase::new(
                "if (true) { 10 }; 3333;",
                vec![10.into(), 3333.into()],
                vec![
                    // 0000
                    code::make(code::Opcode::OpTrue, &vec![]),
                    // 0001
                    code::make(code::Opcode::OpJumpNotTruthy, &vec![7]),
                    // 0004
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    // 0007
                    code::make(code::Opcode::OpPop, &vec![]),
                    // 0008
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    // 0011
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "if (true) { 10 } else { 20 }; 3333;",
                vec![10.into(), 20.into(), 3333.into()],
                vec![
                    // 0000
                    code::make(code::Opcode::OpTrue, &vec![]),
                    // 0001
                    code::make(code::Opcode::OpJumpNotTruthy, &vec![10]),
                    // 0004
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    // 0007
                    code::make(code::Opcode::OpJump, &vec![13]),
                    // 0010
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    // 0013
                    code::make(code::Opcode::OpPop, &vec![]),
                    // 0014
                    code::make(code::Opcode::OpConstant, &vec![2]),
                    // 0017
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
        ];

        run_compiler_tests(tests)
    }
}