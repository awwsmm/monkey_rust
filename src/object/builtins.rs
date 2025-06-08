use crate::object::*;

struct Builtin {
    name: &'static str,
    builtin: BuiltinObj,
}

impl Builtin {
    const fn new(name: &'static str, builtin: BuiltinObj) -> Builtin {
        Self { name, builtin }
    }
}

const BUILTINS: [Builtin; 1] = [
    Builtin::new(
        "len",
        BuiltinObj{
            func: |args| {
                if args.len() != 1 {
                    return new_error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ))
                }

                return match args.get(0) {
                    Some(Object::ArrayObj(arg)) =>
                        Object::IntegerObj(IntegerObj { value: arg.elements.len() as i32 }),

                    Some(Object::StringObj(arg)) =>
                        Object::IntegerObj(IntegerObj { value: arg.value.len() as i32 }),

                    _ =>
                        new_error(format!(
                            "argument to `len` not supported, got {:?}",
                            args.get(0).map(|x| x.object_type())
                        ))
                }
            }
        }
    )
];

fn new_error(string: String) -> Object {
    ErrorObj::new(string).unwrap()
}

pub(crate) fn get_builtin_by_name(name: &str) -> Option<&BuiltinObj> {
    for def in BUILTINS.iter() {
        if def.name == name {
            return Some(&def.builtin)
        }
    }
    None
}