use crate::evaluator::NULL;
use crate::object;
use crate::object::ObjectLike;
use object::builtins::get_builtin_by_name;

pub(crate) struct Builtin{}

impl Builtin {
    pub(crate) fn from(string: &str) -> Option<object::BuiltinObj> {
        match string {
            "len" => get_builtin_by_name(string).cloned(),

            "first" => get_builtin_by_name(string).cloned(),

            "last" => get_builtin_by_name(string).cloned(),

            "rest" => get_builtin_by_name(string).cloned(),

            "push" => Some(object::BuiltinObj{
                func: |args| {
                    if args.len() != 2 {
                        return object::ErrorObj::new(format!(
                            "wrong number of arguments. got={}, want=2",
                            args.len()
                        )).unwrap()
                    };

                    if args.get(0).unwrap().object_type() != object::ObjectType::ArrayObj {
                        return object::ErrorObj::new(format!(
                            "argument to `push` must be ArrayObj, got {:?}",
                            args.get(0).unwrap().object_type()
                        )).unwrap()
                    };

                    let arr = match args.get(0) {
                        Some(object::Object::ArrayObj(inner)) => inner,
                        _ => panic!()
                    };

                    let length = arr.elements.len();

                    let mut new_elements = vec![object::Object::NullObj(object::NullObj{}); length];
                    new_elements.clone_from_slice(&arr.elements);
                    new_elements.push(args.get(1).cloned().unwrap());
                    object::Object::ArrayObj(object::ArrayObj{ elements: new_elements })
                }
            }),

            "puts" => get_builtin_by_name(string).cloned(),

            _ => None,
        }
    }
}