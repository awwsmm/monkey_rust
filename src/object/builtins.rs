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

const BUILTINS: [Builtin; 2] = [
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
                        Some(Object::IntegerObj(IntegerObj { value: arg.elements.len() as i32 })),

                    Some(Object::StringObj(arg)) =>
                        Some(Object::IntegerObj(IntegerObj { value: arg.value.len() as i32 })),

                    _ =>
                        new_error(format!(
                            "argument to `len` not supported, got {:?}",
                            args.get(0).map(|x| x.object_type())
                        ))
                }
            }
        }
    ),
    Builtin::new(
        "puts",
        BuiltinObj{
            func: |args| {
                for arg in args.iter() {
                    println!("{}", arg.inspect())
                }

                None
            }
        }
    )
];

fn new_error(string: String) -> Option<Object> {
    ErrorObj::new(string)
}

pub(crate) fn get_builtin_by_name(name: &str) -> Option<&BuiltinObj> {
    for def in BUILTINS.iter() {
        if def.name == name {
            return Some(&def.builtin)
        }
    }
    None
}