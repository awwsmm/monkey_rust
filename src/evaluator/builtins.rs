use crate::evaluator;

pub(crate) struct Builtin{}

impl Builtin {
    pub(crate) fn from(string: &str) -> Option<evaluator::object::BuiltinObj> {
        match string {
            "len" => Some(evaluator::object::BuiltinObj{
                func: |x| evaluator::NULL.unwrap()
            }) ,
            _ => None,
        }
    }
}