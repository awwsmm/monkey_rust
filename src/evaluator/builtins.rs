use crate::object;
use object::builtins::get_builtin_by_name;

pub(crate) struct Builtin{}

impl Builtin {
    pub(crate) fn from(string: &str) -> Option<object::BuiltinObj> {
        match string {
            "len" => get_builtin_by_name(string).cloned(),

            "first" => get_builtin_by_name(string).cloned(),

            "last" => get_builtin_by_name(string).cloned(),

            "rest" => get_builtin_by_name(string).cloned(),

            "push" => get_builtin_by_name(string).cloned(),

            "puts" => get_builtin_by_name(string).cloned(),

            _ => None,
        }
    }
}