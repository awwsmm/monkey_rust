use crate::{code, object};

struct Frame {
    func: object::CompiledFunctionObj,
    ip: i32,
}

impl Frame {
    fn new(func: object::CompiledFunctionObj) -> Self {
        Self { func, ip: -1 }
    }

    fn instructions(&self) -> code::Instructions {
        self.func.instructions.clone()
    }
}