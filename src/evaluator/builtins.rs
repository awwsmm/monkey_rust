use crate::object;
use object::builtins::get_builtin_by_name;

pub(crate) struct Builtin{}

impl Builtin {
    pub(crate) fn from(string: &str) -> Option<object::BuiltinObj> {
        get_builtin_by_name(string).cloned()
    }
}