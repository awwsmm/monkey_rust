use crate::{code, object};

pub(crate) struct Frame {
    func: object::CompiledFunctionObj,
    pub(crate) ip: i32,
}

impl Frame {
    pub(crate) fn new(func: object::CompiledFunctionObj) -> Self {
        Self { func, ip: -1 }
    }

    pub(crate) fn instructions(&self) -> code::Instructions {
        self.func.instructions.clone()
    }
}