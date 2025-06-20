pub(crate) mod symbol_table;

use crate::compiler::symbol_table::{Symbol, SymbolTable, BUILTIN_SCOPE, FREE_SCOPE, FUNCTION_SCOPE, GLOBAL_SCOPE, LOCAL_SCOPE};
use crate::{ast, code, object};
use std::cmp::PartialEq;
use std::fmt::{Display, Formatter};

pub(crate) struct Error {
    pub message: String
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

struct CompilationScope {
    instructions: code::Instructions,
    last_instruction: EmittedInstruction,
    previous_instruction: EmittedInstruction,
}

pub struct Compiler {
    constants: Vec<object::Object>,

    pub(crate) symbol_table: SymbolTable,

    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

impl Compiler {
    pub fn new() -> Self {
        let main_scope = CompilationScope{
            instructions: code::Instructions(vec![]),
            last_instruction: EmittedInstruction { opcode: None, position: 0 },
            previous_instruction: EmittedInstruction { opcode: None, position: 0 },
        };

        let mut symbol_table = SymbolTable::new();

        for (i, v) in object::builtins::BUILTINS.iter().enumerate() {
            symbol_table.define_builtin(i, v.name);
        }

        Self {
            constants: vec![],
            symbol_table,
            scopes: vec![main_scope],
            scope_index: 0,
        }
    }

    pub(crate) fn new_with_state(s: SymbolTable, constants: Vec<object::Object>) -> Self {
        let mut compiler = Self::new();
        compiler.symbol_table = s;
        compiler.constants = constants;
        compiler
    }

    pub fn compile(&mut self, node: ast::Node) -> Option<Error> {
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

            ast::Node::Statement(ast::Statement::LetStatement(node)) => {
                let symbol = self.symbol_table.define(node.name?.value);
                let err = self.compile(ast::Node::Expression(node.value?));
                if err.is_some() {
                    return err
                }

                if symbol.scope == GLOBAL_SCOPE {
                    self.emit(code::Opcode::OpSetGlobal, vec![symbol.index]);
                } else {
                    self.emit(code::Opcode::OpSetLocal, vec![symbol.index]);
                }
            }

            ast::Node::Statement(ast::Statement::ReturnStatement(node)) => {
                let err = self.compile(ast::Node::Expression(node.return_value?));
                if err.is_some() {
                    return err
                }
                self.emit(code::Opcode::OpReturnValue, vec![]);
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

                if self.last_instruction_is(code::Opcode::OpPop) {
                    self.remove_last_pop()
                }

                // Emit an `OpJump` with a bogus value
                let jump_pos = self.emit(code::Opcode::OpJump, vec![9999]);

                let after_consequence_pos = self.current_instructions().len();
                self.change_operand(jump_not_truthy_pos, after_consequence_pos);

                if node.alternative.is_none() {
                    self.emit(code::Opcode::OpNull, vec![]);
                } else {
                    let err = self.compile(ast::Node::Statement(ast::Statement::BlockStatement(node.alternative?)));
                    if err.is_some() {
                        return err
                    }

                    if self.last_instruction_is(code::Opcode::OpPop) {
                        self.remove_last_pop()
                    }
                }

                let after_alternative_pos = self.current_instructions().len();
                self.change_operand(jump_pos, after_alternative_pos)
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
                let obj = object::Object::IntegerObj(object::IntegerObj{ value: node.value });
                let constant = self.add_constant(obj);
                self.emit(code::Opcode::OpConstant, vec![constant]);
            }

            ast::Node::Expression(ast::Expression::StringLiteral(node)) => {
                let obj = object::Object::StringObj(object::StringObj{ value: node.value });
                let constant = self.add_constant(obj);
                self.emit(code::Opcode::OpConstant, vec![constant]);
            }

            ast::Node::Expression(ast::Expression::ArrayLiteral(node)) => {
                let len = node.elements.len();

                for element in node.elements.into_iter() {
                    let err = self.compile(ast::Node::Expression(element));
                    if err.is_some() {
                        return err
                    }
                }

                self.emit(code::Opcode::OpArray, vec![len]);
            }

            ast::Node::Expression(ast::Expression::HashLiteral(node)) => {
                let mut keys = vec![];
                for (k, _) in node.pairs.iter() {
                    keys.push(k)
                }
                keys.sort_by_key(|k| k.to_string());

                let len = node.pairs.len();

                for k in keys.into_iter() {
                    let err = self.compile(ast::Node::Expression(k.clone()));
                    if err.is_some() {
                        return err
                    }

                    let err = self.compile(ast::Node::Expression(node.pairs[k].clone()));
                    if err.is_some() {
                        return err
                    }
                }

                self.emit(code::Opcode::OpHash, vec![len * 2]);
            }

            ast::Node::Expression(ast::Expression::Boolean(node)) => {
                if node.value {
                    self.emit(code::Opcode::OpTrue, vec![])
                } else {
                    self.emit(code::Opcode::OpFalse, vec![])
                };
            }

            ast::Node::Expression(ast::Expression::Identifier(node)) => {
                let symbol = match self.symbol_table.resolve(node.value.as_str()) {
                    None => return Error::new(format!("undefined variable {}", node.value)),
                    Some(symbol) => symbol
                };

                self.load_symbol(symbol);
            }

            ast::Node::Expression(ast::Expression::IndexExpression(node)) => {
                let err = self.compile(ast::Node::Expression(*node.left));
                if err.is_some() {
                    return err
                }

                let err = self.compile(ast::Node::Expression(*node.index?));
                if err.is_some() {
                    return err
                }

                self.emit(code::Opcode::OpIndex, vec![]);
            }

            ast::Node::Expression(ast::Expression::FunctionLiteral(node)) => {
                self.enter_scope();

                if node.name != "" {
                    self.symbol_table.define_function_name(node.name.as_str());
                }

                for p in node.parameters.iter() {
                    self.symbol_table.define(p.value.clone());
                }

                let err = self.compile(ast::Node::Statement(ast::Statement::BlockStatement(node.body?)));
                if err.is_some() {
                    return err
                }

                if self.last_instruction_is(code::Opcode::OpPop) {
                    self.replace_last_pop_with_return()
                }
                if !self.last_instruction_is(code::Opcode::OpReturnValue) {
                    self.emit(code::Opcode::OpReturn, vec![]);
                }

                let free_symbols = self.symbol_table.free_symbols.clone();
                let num_locals = self.symbol_table.num_definitions;
                let instructions = self.leave_scope();

                let free_symbols_len = free_symbols.len();
                for s in free_symbols.into_iter() {
                    self.load_symbol(s)
                }

                let compiled_func = object::Object::CompiledFunctionObj(
                    object::CompiledFunctionObj{ instructions, num_locals, num_parameters: node.parameters.len() }
                );
                let func_index = self.add_constant(compiled_func);
                self.emit(code::Opcode::OpClosure, vec![func_index, free_symbols_len]);
            }

            ast::Node::Expression(ast::Expression::CallExpression(node)) => {
                let err = self.compile(ast::Node::Expression(*node.function));
                if err.is_some() {
                    return err
                }

                let len = node.arguments.len();

                for a in node.arguments.into_iter() {
                    let err = self.compile(ast::Node::Expression(a));
                    if err.is_some() {
                        return err
                    }
                }

                self.emit(code::Opcode::OpCall, vec![len]);
            }

            _ => ()
        }

        None
    }

    fn replace_last_pop_with_return(&mut self) {
        let last_pos = self.scopes[self.scope_index].last_instruction.position;
        self.replace_instruction(last_pos, code::make(code::Opcode::OpReturnValue, &vec![]));

        self.scopes[self.scope_index].last_instruction.opcode = Some(code::Opcode::OpReturnValue)
    }

    fn current_instructions(&self) -> code::Instructions {
        self.scopes[self.scope_index].instructions.clone()
    }

    fn last_instruction_is(&mut self, op: code::Opcode) -> bool {
        if self.current_instructions().len() == 0 {
            return false
        }

        self.scopes[self.scope_index].last_instruction.opcode == Some(op)
    }

    fn remove_last_pop(&mut self) {
        let last = self.scopes[self.scope_index].last_instruction;
        let previous = self.scopes[self.scope_index].previous_instruction;

        let old = self.current_instructions();
        let new = &old.0[..last.position];

        self.scopes[self.scope_index].instructions = code::Instructions(Vec::from(new));
        self.scopes[self.scope_index].last_instruction = previous;
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.current_instructions().clone(),
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
        let previous = self.scopes[self.scope_index].last_instruction;
        let last = EmittedInstruction{ opcode: Some(op), position: pos };

        self.scopes[self.scope_index].previous_instruction = previous;
        self.scopes[self.scope_index].last_instruction = last;
    }

    fn add_instruction(&mut self, mut ins: Vec<u8>) -> usize {
        let pos_new_instruction = self.current_instructions().len();
        let mut updated_instructions = self.current_instructions().0.clone();
        updated_instructions.append(&mut ins);

        self.scopes[self.scope_index].instructions = code::Instructions(updated_instructions);

        pos_new_instruction
    }

    fn replace_instruction(&mut self, pos: usize, new_instruction: Vec<u8>) {
        let mut ins = self.current_instructions();

        for (i, byte) in new_instruction.iter().enumerate() {
            ins[pos+i] = *byte;
        }

        self.scopes[self.scope_index].instructions = ins;
    }

    fn change_operand(&mut self, op_pos: usize, operand: usize) {
        let op: code::Opcode = self.current_instructions()[op_pos].into();
        let new_instruction = code::make(op, &vec![operand]);

        self.replace_instruction(op_pos, new_instruction)
    }

    fn enter_scope(&mut self) {
        let scope = CompilationScope{
            instructions: code::Instructions(vec![]),
            last_instruction: EmittedInstruction { opcode: None, position: 0 },
            previous_instruction: EmittedInstruction { opcode: None, position: 0 },
        };
        self.scopes.push(scope);
        self.symbol_table = SymbolTable::new_enclosed(Box::new(self.symbol_table.clone()));
        self.scope_index += 1;
    }

    fn leave_scope(&mut self) -> code::Instructions {
        let instructions = self.current_instructions();

        self.scopes.pop();
        self.scope_index -= 1;
        self.symbol_table = *(self.symbol_table.outer.clone().unwrap());

        instructions
    }

    fn load_symbol(&mut self, s: Symbol) {
        match s.scope {
            GLOBAL_SCOPE => self.emit(code::Opcode::OpGetGlobal, vec![s.index]),
            LOCAL_SCOPE => self.emit(code::Opcode::OpGetLocal, vec![s.index]),
            BUILTIN_SCOPE => self.emit(code::Opcode::OpGetBuiltin, vec![s.index]),
            FREE_SCOPE => self.emit(code::Opcode::OpGetFree, vec![s.index]),
            FUNCTION_SCOPE => self.emit(code::Opcode::OpCurrentClosure, vec![]),
            _ => panic!(),
        };
    }
}

#[derive(Clone)]
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

                Expected::String(String(constant)) => {
                    let err = test_string_object(constant, actual.get(i).cloned()?);
                    if let Some(err) = err {
                        return Error::new(format!(
                            "constant {} - test_string_object failed: {}", i, err
                        ))
                    }
                }

                Expected::Instructions(Instructions(constant)) => {
                    let func = match actual.get(i).cloned()? {
                        object::Object::CompiledFunctionObj(obj) => obj,
                        _ => return Error::new(format!(
                            "constant {} - not a function: {:?}",
                            i, actual[i]
                        ))
                    };

                    let err = test_instructions(constant, func.instructions);
                    if let Some(err) = err {
                        return Error::new(format!(
                            "constant {} - test_instructions failed: {}", i, err
                        ))
                    }
                }

                _ => () // TODO
            }
        }

        None
    }

    fn test_string_object(expected: &'static str, actual: object::Object) -> Option<Error> {
        let result = match actual {
            object::Object::StringObj(obj) => obj,
            _ => return Error::new(format!(
                "object is not String. got={:?}", actual
            )),
        };

        if result.value != expected {
            return Error::new(format!(
                "object has wrong value. got={}, want={}", result.value, expected
            ))
        }

        None
    }

    fn test_integer_object(expected: i32, actual: object::Object) -> Option<Error> {
        let result = match actual {
            object::Object::IntegerObj(obj) => obj,
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
        String(String),
        Instructions(Instructions),
    }

    struct Integer(i32);
    struct Boolean(bool);
    struct String(&'static str);
    struct Instructions(Vec<code::Instructions>);

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

    impl Into<Expected> for &'static str {
        fn into(self) -> Expected {
            Expected::String(String(self))
        }
    }

    impl Into<Expected> for Vec<Vec<u8>> {
        fn into(self) -> Expected {
            Expected::Instructions(Instructions(self.into_iter().map(code::Instructions).collect()))
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
    fn test_conditionals() {
        let tests = vec![
            CompilerTestCase::new(
                "if (true) { 10 }; 3333;",
                vec![10.into(), 3333.into()],
                vec![
                    // 0000
                    code::make(code::Opcode::OpTrue, &vec![]),
                    // 0001
                    code::make(code::Opcode::OpJumpNotTruthy, &vec![10]),
                    // 0004
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    // 0007
                    code::make(code::Opcode::OpJump, &vec![11]),
                    // 0010
                    code::make(code::Opcode::OpNull, &vec![]),
                    // 0011
                    code::make(code::Opcode::OpPop, &vec![]),
                    // 0012
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    // 0015
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

    #[test]
    fn test_global_let_statements() {
        let tests = vec![
            CompilerTestCase::new(
                r#"
                let one = 1;
                let two = 2;
                "#,
                vec![1.into(), 2.into()],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpSetGlobal, &vec![0]),
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    code::make(code::Opcode::OpSetGlobal, &vec![1]),
                ],
            ),
            CompilerTestCase::new(
                r#"
                let one = 1;
                one;
                "#,
                vec![1.into()],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpSetGlobal, &vec![0]),
                    code::make(code::Opcode::OpGetGlobal, &vec![0]),
                    code::make(code::Opcode::OpPop, &vec![]),

                ],
            ),
            CompilerTestCase::new(
                r#"
                let one = 1;
                let two = one;
                two;
                "#,
                vec![1.into()],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpSetGlobal, &vec![0]),
                    code::make(code::Opcode::OpGetGlobal, &vec![0]),
                    code::make(code::Opcode::OpSetGlobal, &vec![1]),
                    code::make(code::Opcode::OpGetGlobal, &vec![1]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_string_expressions() {
        let tests = vec![
            CompilerTestCase::new(
                r#""monkey""#,
                vec!["monkey".into()],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                r#""mon" + "key""#,
                vec!["mon".into(), "key".into()],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    code::make(code::Opcode::OpAdd, &vec![]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_array_literals() {
        let tests = vec![
            CompilerTestCase::new(
                "[]",
                vec![],
                vec![
                    code::make(code::Opcode::OpArray, &vec![0]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "[1, 2, 3]",
                vec![1.into(), 2.into(), 3.into()],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    code::make(code::Opcode::OpConstant, &vec![2]),
                    code::make(code::Opcode::OpArray, &vec![3]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "[1 + 2, 3 - 4, 5 * 6]",
                vec![1.into(), 2.into(), 3.into(), 4.into(), 5.into(), 6.into()],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    code::make(code::Opcode::OpAdd, &vec![]),
                    code::make(code::Opcode::OpConstant, &vec![2]),
                    code::make(code::Opcode::OpConstant, &vec![3]),
                    code::make(code::Opcode::OpSub, &vec![]),
                    code::make(code::Opcode::OpConstant, &vec![4]),
                    code::make(code::Opcode::OpConstant, &vec![5]),
                    code::make(code::Opcode::OpMul, &vec![]),
                    code::make(code::Opcode::OpArray, &vec![3]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_hash_literals() {
        let tests = vec![
            CompilerTestCase::new(
                "{}",
                vec![],
                vec![
                    code::make(code::Opcode::OpHash, &vec![0]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "{1: 2, 3: 4, 5: 6}",
                vec![1.into(), 2.into(), 3.into(), 4.into(), 5.into(), 6.into()],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    code::make(code::Opcode::OpConstant, &vec![2]),
                    code::make(code::Opcode::OpConstant, &vec![3]),
                    code::make(code::Opcode::OpConstant, &vec![4]),
                    code::make(code::Opcode::OpConstant, &vec![5]),
                    code::make(code::Opcode::OpHash, &vec![6]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "{1: 2 + 3, 4: 5 * 6}",
                vec![1.into(), 2.into(), 3.into(), 4.into(), 5.into(), 6.into()],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    code::make(code::Opcode::OpConstant, &vec![2]),
                    code::make(code::Opcode::OpAdd, &vec![]),
                    code::make(code::Opcode::OpConstant, &vec![3]),
                    code::make(code::Opcode::OpConstant, &vec![4]),
                    code::make(code::Opcode::OpConstant, &vec![5]),
                    code::make(code::Opcode::OpMul, &vec![]),
                    code::make(code::Opcode::OpHash, &vec![4]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_index_expressions() {
        let tests = vec![
            CompilerTestCase::new(
                "[1, 2, 3][1 + 1]",
                vec![1.into(), 2.into(), 3.into(), 1.into(), 1.into()],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    code::make(code::Opcode::OpConstant, &vec![2]),
                    code::make(code::Opcode::OpArray, &vec![3]),
                    code::make(code::Opcode::OpConstant, &vec![3]),
                    code::make(code::Opcode::OpConstant, &vec![4]),
                    code::make(code::Opcode::OpAdd, &vec![]),
                    code::make(code::Opcode::OpIndex, &vec![]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "{1: 2}[2 - 1]",
                vec![1.into(), 2.into(), 2.into(), 1.into()],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    code::make(code::Opcode::OpHash, &vec![2]),
                    code::make(code::Opcode::OpConstant, &vec![2]),
                    code::make(code::Opcode::OpConstant, &vec![3]),
                    code::make(code::Opcode::OpSub, &vec![]),
                    code::make(code::Opcode::OpIndex, &vec![]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_functions() {
        let tests = vec![
            CompilerTestCase::new(
                "fn() { return 5 + 10 }",
                vec![
                    5.into(),
                    10.into(),
                    vec![
                        code::make(code::Opcode::OpConstant, &vec![0]),
                        code::make(code::Opcode::OpConstant, &vec![1]),
                        code::make(code::Opcode::OpAdd, &vec![]),
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                ],
                vec![
                    code::make(code::Opcode::OpClosure, &vec![2, 0]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "fn() { 5 + 10 }",
                vec![
                    5.into(),
                    10.into(),
                    vec![
                        code::make(code::Opcode::OpConstant, &vec![0]),
                        code::make(code::Opcode::OpConstant, &vec![1]),
                        code::make(code::Opcode::OpAdd, &vec![]),
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                ],
                vec![
                    code::make(code::Opcode::OpClosure, &vec![2, 0]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "fn() { 1; 2 }",
                vec![
                    1.into(),
                    2.into(),
                    vec![
                        code::make(code::Opcode::OpConstant, &vec![0]),
                        code::make(code::Opcode::OpPop, &vec![]),
                        code::make(code::Opcode::OpConstant, &vec![1]),
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                ],
                vec![
                    code::make(code::Opcode::OpClosure, &vec![2, 0]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_compiler_scopes() {
        let mut should_panic = false;

        let mut compiler = Compiler::new();
        if compiler.scope_index != 0 {
            should_panic = true;
            eprintln!("scope_index wrong. got={}, want={}", compiler.scope_index, 0)
        }
        let global_symbol_table = compiler.symbol_table.clone();

        compiler.emit(code::Opcode::OpMul, vec![]);

        compiler.enter_scope();
        if compiler.scope_index != 1 {
            should_panic = true;
            eprintln!("scope_index wrong. got={}, want={}", compiler.scope_index, 1)
        }

        compiler.emit(code::Opcode::OpSub, vec![]);

        let len = compiler.scopes[compiler.scope_index].instructions.len();
        if len != 1 {
            should_panic = true;
            eprintln!("instructions length wrong. got={}", len)
        }

        let last = compiler.scopes[compiler.scope_index].last_instruction;
        if last.opcode != Some(code::Opcode::OpSub) {
            should_panic = true;
            eprintln!("last_instruction.opcode wrong. got={:?}, want={:?}",
                last.opcode, Some(code::Opcode::OpSub))
        }

        let symbol_table = *(compiler.symbol_table.outer.clone().unwrap());
        if symbol_table != global_symbol_table {
            should_panic = true;
            eprintln!("compiler did not enclose symbol_table")
        }

        compiler.leave_scope();
        if compiler.scope_index != 0 {
            should_panic = true;
            eprintln!("scope_index wrong. got={}, want={}", compiler.scope_index, 0)
        }

        let symbol_table = compiler.symbol_table.clone();
        if symbol_table != global_symbol_table {
            should_panic = true;
            eprintln!("compiler did not restore global symbol table")
        }
        if symbol_table.outer.is_some() {
            should_panic = true;
            eprintln!("compiler modified global symbol table incorrectly")
        }

        compiler.emit(code::Opcode::OpAdd, vec![]);

        let len = compiler.scopes[compiler.scope_index].instructions.len();
        if len != 2 {
            should_panic = true;
            eprintln!("instructions length wrong. got={}", len)
        }

        let last = compiler.scopes[compiler.scope_index].last_instruction;
        if last.opcode != Some(code::Opcode::OpAdd) {
            should_panic = true;
            eprintln!("last_instruction.opcode wrong. got={:?}, want={:?}",
                last.opcode, Some(code::Opcode::OpAdd))
        }

        let previous = compiler.scopes[compiler.scope_index].previous_instruction;
        if previous.opcode != Some(code::Opcode::OpMul) {
            should_panic = true;
            eprintln!("previous_instruction.opcode wrong. got={:?}, want={:?}",
                      previous.opcode, Some(code::Opcode::OpMul))
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_functions_without_return_value() {
        let tests = vec![
            CompilerTestCase::new(
                "fn() { }",
                vec![
                    vec![
                        code::make(code::Opcode::OpReturn, &vec![]),
                    ].into(),
                ],
                vec![
                    code::make(code::Opcode::OpClosure, &vec![0, 0]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_function_calls() {
        let tests = vec![
            CompilerTestCase::new(
                "fn() { 24 }();",
                vec![
                    24.into(),
                    vec![
                        code::make(code::Opcode::OpConstant, &vec![0]), // The literal "24"
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                ],
                vec![
                    code::make(code::Opcode::OpClosure, &vec![1, 0]),
                    code::make(code::Opcode::OpCall, &vec![0]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                r#"
                let noArg = fn() { 24 };
                noArg();
                "#,
                vec![
                    24.into(),
                    vec![
                        code::make(code::Opcode::OpConstant, &vec![0]), // The literal "24"
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                ],
                vec![
                    code::make(code::Opcode::OpClosure, &vec![1, 0]),
                    code::make(code::Opcode::OpSetGlobal, &vec![0]),
                    code::make(code::Opcode::OpGetGlobal, &vec![0]),
                    code::make(code::Opcode::OpCall, &vec![0]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                r#"
                let oneArg = fn(a) { a };
                oneArg(24);
                "#,
                vec![
                    vec![
                        code::make(code::Opcode::OpGetLocal, &vec![0]),
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                    24.into(),
                ],
                vec![
                    code::make(code::Opcode::OpClosure, &vec![0, 0]),
                    code::make(code::Opcode::OpSetGlobal, &vec![0]),
                    code::make(code::Opcode::OpGetGlobal, &vec![0]),
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    code::make(code::Opcode::OpCall, &vec![1]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                r#"
                let manyArg = fn(a, b, c) { a; b; c };
                manyArg(24, 25, 26);
                "#,
                vec![
                    vec![
                        code::make(code::Opcode::OpGetLocal, &vec![0]),
                        code::make(code::Opcode::OpPop, &vec![]),
                        code::make(code::Opcode::OpGetLocal, &vec![1]),
                        code::make(code::Opcode::OpPop, &vec![]),
                        code::make(code::Opcode::OpGetLocal, &vec![2]),
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                    24.into(),
                    25.into(),
                    26.into(),
                ],
                vec![
                    code::make(code::Opcode::OpClosure, &vec![0, 0]),
                    code::make(code::Opcode::OpSetGlobal, &vec![0]),
                    code::make(code::Opcode::OpGetGlobal, &vec![0]),
                    code::make(code::Opcode::OpConstant, &vec![1]),
                    code::make(code::Opcode::OpConstant, &vec![2]),
                    code::make(code::Opcode::OpConstant, &vec![3]),
                    code::make(code::Opcode::OpCall, &vec![3]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_let_statement_scopes() {
        let tests = vec![
            CompilerTestCase::new(
                r#"
                let num = 55;
                fn() { num }
                "#,
                vec![
                    55.into(),
                    vec![
                        code::make(code::Opcode::OpGetGlobal, &vec![0]),
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                ],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpSetGlobal, &vec![0]),
                    code::make(code::Opcode::OpClosure, &vec![1, 0]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                r#"
                fn() {
                    let num = 55;
                    num
                }
                "#,
                vec![
                    55.into(),
                    vec![
                        code::make(code::Opcode::OpConstant, &vec![0]),
                        code::make(code::Opcode::OpSetLocal, &vec![0]),
                        code::make(code::Opcode::OpGetLocal, &vec![0]),
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                ],
                vec![
                    code::make(code::Opcode::OpClosure, &vec![1, 0]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                r#"
                fn() {
                    let a = 55;
                    let b = 77;
                    a + b
                }
                "#,
                vec![
                    55.into(),
                    77.into(),
                    vec![
                        code::make(code::Opcode::OpConstant, &vec![0]),
                        code::make(code::Opcode::OpSetLocal, &vec![0]),
                        code::make(code::Opcode::OpConstant, &vec![1]),
                        code::make(code::Opcode::OpSetLocal, &vec![1]),
                        code::make(code::Opcode::OpGetLocal, &vec![0]),
                        code::make(code::Opcode::OpGetLocal, &vec![1]),
                        code::make(code::Opcode::OpAdd, &vec![]),
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                ],
                vec![
                    code::make(code::Opcode::OpClosure, &vec![2, 0]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_builtins() {
        let tests = vec![
            CompilerTestCase::new(
                r#"
                len([]);
                push([], 1);
                "#,
                vec![
                    1.into(),
                ],
                vec![
                    code::make(code::Opcode::OpGetBuiltin, &vec![0]),
                    code::make(code::Opcode::OpArray, &vec![0]),
                    code::make(code::Opcode::OpCall, &vec![1]),
                    code::make(code::Opcode::OpPop, &vec![]),
                    code::make(code::Opcode::OpGetBuiltin, &vec![5]),
                    code::make(code::Opcode::OpArray, &vec![0]),
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpCall, &vec![2]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                "fn() { len([]) }",
                vec![
                    vec![
                        code::make(code::Opcode::OpGetBuiltin, &vec![0]),
                        code::make(code::Opcode::OpArray, &vec![0]),
                        code::make(code::Opcode::OpCall, &vec![1]),
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                ],
                vec![
                    code::make(code::Opcode::OpClosure, &vec![0, 0]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_closures() {
        let tests = vec![
            CompilerTestCase::new(
                r#"
                fn(a) {
                    fn(b) {
                        a + b
                    }
                }
                "#,
                vec![
                    vec![
                        code::make(code::Opcode::OpGetFree, &vec![0]),
                        code::make(code::Opcode::OpGetLocal, &vec![0]),
                        code::make(code::Opcode::OpAdd, &vec![]),
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                    vec![
                        code::make(code::Opcode::OpGetLocal, &vec![0]),
                        code::make(code::Opcode::OpClosure, &vec![0, 1]),
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                ],
                vec![
                    code::make(code::Opcode::OpClosure, &vec![1, 0]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                r#"
                fn(a) {
                    fn(b) {
                        fn(c) {
                            a + b + c
                        }
                    }
                }
                "#,
                vec![
                    vec![
                        code::make(code::Opcode::OpGetFree, &vec![0]),
                        code::make(code::Opcode::OpGetFree, &vec![1]),
                        code::make(code::Opcode::OpAdd, &vec![]),
                        code::make(code::Opcode::OpGetLocal, &vec![0]),
                        code::make(code::Opcode::OpAdd, &vec![]),
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                    vec![
                        code::make(code::Opcode::OpGetFree, &vec![0]),
                        code::make(code::Opcode::OpGetLocal, &vec![0]),
                        code::make(code::Opcode::OpClosure, &vec![0, 2]),
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                    vec![
                        code::make(code::Opcode::OpGetLocal, &vec![0]),
                        code::make(code::Opcode::OpClosure, &vec![1, 1]),
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                ],
                vec![
                    code::make(code::Opcode::OpClosure, &vec![2, 0]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                r#"
                let global = 55;

                fn() {
                    let a = 66;

                    fn() {
                        let b = 77;

                        fn() {
                            let c = 88;

                            global + a + b + c
                        }
                    }
                }
                "#,
                vec![
                    55.into(),
                    66.into(),
                    77.into(),
                    88.into(),
                    vec![
                        code::make(code::Opcode::OpConstant, &vec![3]),
                        code::make(code::Opcode::OpSetLocal, &vec![0]),
                        code::make(code::Opcode::OpGetGlobal, &vec![0]),
                        code::make(code::Opcode::OpGetFree, &vec![0]),
                        code::make(code::Opcode::OpAdd, &vec![]),
                        code::make(code::Opcode::OpGetFree, &vec![1]),
                        code::make(code::Opcode::OpAdd, &vec![]),
                        code::make(code::Opcode::OpGetLocal, &vec![0]),
                        code::make(code::Opcode::OpAdd, &vec![]),
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                    vec![
                        code::make(code::Opcode::OpConstant, &vec![2]),
                        code::make(code::Opcode::OpSetLocal, &vec![0]),
                        code::make(code::Opcode::OpGetFree, &vec![0]),
                        code::make(code::Opcode::OpGetLocal, &vec![0]),
                        code::make(code::Opcode::OpClosure, &vec![4, 2]),
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                    vec![
                        code::make(code::Opcode::OpConstant, &vec![1]),
                        code::make(code::Opcode::OpSetLocal, &vec![0]),
                        code::make(code::Opcode::OpGetLocal, &vec![0]),
                        code::make(code::Opcode::OpClosure, &vec![5, 1]),
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                ],
                vec![
                    code::make(code::Opcode::OpConstant, &vec![0]),
                    code::make(code::Opcode::OpSetGlobal, &vec![0]),
                    code::make(code::Opcode::OpClosure, &vec![6, 0]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_recursive_functions() {
        let tests = vec![
            CompilerTestCase::new(
                r#"
                let countDown = fn(x) { countDown(x - 1); };
			    countDown(1);
                "#,
                vec![
                    1.into(),
                    vec![
                        code::make(code::Opcode::OpCurrentClosure, &vec![]),
                        code::make(code::Opcode::OpGetLocal, &vec![0]),
                        code::make(code::Opcode::OpConstant, &vec![0]),
                        code::make(code::Opcode::OpSub, &vec![]),
                        code::make(code::Opcode::OpCall, &vec![1]),
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                    1.into(),
                ],
                vec![
                    code::make(code::Opcode::OpClosure, &vec![1, 0]),
                    code::make(code::Opcode::OpSetGlobal, &vec![0]),
                    code::make(code::Opcode::OpGetGlobal, &vec![0]),
                    code::make(code::Opcode::OpConstant, &vec![2]),
                    code::make(code::Opcode::OpCall, &vec![1]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
            CompilerTestCase::new(
                r#"
                let wrapper = fn() {
                    let countDown = fn(x) { countDown(x - 1); };
                    countDown(1);
                };
                wrapper();
                "#,
                vec![
                    1.into(),
                    vec![
                        code::make(code::Opcode::OpCurrentClosure, &vec![]),
                        code::make(code::Opcode::OpGetLocal, &vec![0]),
                        code::make(code::Opcode::OpConstant, &vec![0]),
                        code::make(code::Opcode::OpSub, &vec![]),
                        code::make(code::Opcode::OpCall, &vec![1]),
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                    1.into(),
                    vec![
                        code::make(code::Opcode::OpClosure, &vec![1, 0]),
                        code::make(code::Opcode::OpSetLocal, &vec![0]),
                        code::make(code::Opcode::OpGetLocal, &vec![0]),
                        code::make(code::Opcode::OpConstant, &vec![2]),
                        code::make(code::Opcode::OpCall, &vec![1]),
                        code::make(code::Opcode::OpReturnValue, &vec![]),
                    ].into(),
                ],
                vec![
                    code::make(code::Opcode::OpClosure, &vec![3, 0]),
                    code::make(code::Opcode::OpSetGlobal, &vec![0]),
                    code::make(code::Opcode::OpGetGlobal, &vec![0]),
                    code::make(code::Opcode::OpCall, &vec![0]),
                    code::make(code::Opcode::OpPop, &vec![]),
                ],
            ),
        ];

        run_compiler_tests(tests)
    }
}