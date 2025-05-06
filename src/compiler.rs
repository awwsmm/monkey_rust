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