use crate::{code, object};

pub(crate) struct Frame {
    func: object::CompiledFunctionObj,
    pub(crate) ip: i32,
    pub(crate) base_pointer: i32,
}

impl Frame {
    pub(crate) fn new(func: object::CompiledFunctionObj, base_pointer: i32) -> Self {
        Self {
            func,
            ip: -1,
            base_pointer,
        }
    }

    pub(crate) fn instructions(&self) -> code::Instructions {
        self.func.instructions.clone()
    }
}