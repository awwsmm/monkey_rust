use crate::evaluator;

struct Builtin{}

impl Builtin {
    fn from(string: &str) -> Option<evaluator::object::BuiltinObj> {
        match string {
            "len" => Some(evaluator::object::BuiltinObj{
                func: |x| evaluator::NULL.unwrap()
            }) ,
            _ => None,
        }
    }
}