use crate::{code, object};

pub(crate) struct Frame {
    cl: object::ClosureObj,
    pub(crate) ip: i32,
    pub(crate) base_pointer: i32,
}

impl Frame {
    pub(crate) fn new(cl: object::ClosureObj, base_pointer: i32) -> Self {
        Self {
            cl,
            ip: -1,
            base_pointer,
        }
    }

    pub(crate) fn instructions(&self) -> code::Instructions {
        self.cl.func.instructions.clone()
    }
}