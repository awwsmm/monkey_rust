use crate::object::*;

pub(crate) struct Builtin {
    pub(crate) name: &'static str,
    pub(crate) builtin: BuiltinObj,
}

impl Builtin {
    const fn new(name: &'static str, builtin: BuiltinObj) -> Builtin {
        Self { name, builtin }
    }
}

pub(crate) const BUILTINS: [Builtin; 6] = [
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
    ),
    Builtin::new(
        "first",
        BuiltinObj{
            func: |args| {
                if args.len() != 1 {
                    return new_error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ))
                };

                if args.get(0).unwrap().object_type() != ObjectType::ArrayObj {
                    return new_error(format!(
                        "argument to `first` must be ArrayObj, got {:?}",
                        args.get(0).unwrap().object_type()
                    ))
                };

                let arr = match args.get(0) {
                    Some(Object::ArrayObj(inner)) => inner,
                    _ => panic!()
                };

                if arr.elements.len() > 0 {
                    return arr.elements.get(0).cloned()
                }

                None
            }
        }
    ),
    Builtin::new(
        "last",
        BuiltinObj{
            func: |args| {
                if args.len() != 1 {
                    return new_error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ))
                };

                if args.get(0).unwrap().object_type() != ObjectType::ArrayObj {
                    return new_error(format!(
                        "argument to `last` must be ArrayObj, got {:?}",
                        args.get(0).unwrap().object_type()
                    ))
                };

                let arr = match args.get(0) {
                    Some(Object::ArrayObj(inner)) => inner,
                    _ => panic!()
                };

                let length = arr.elements.len();

                if length > 0 {
                    return arr.elements.get(length-1).cloned()
                }

                None
            }
        }
    ),
    Builtin::new(
        "rest",
        BuiltinObj{
            func: |args| {
                if args.len() != 1 {
                    return new_error(format!(
                        "wrong number of arguments. got={}, want=1",
                        args.len()
                    ))
                };

                if args.get(0).unwrap().object_type() != ObjectType::ArrayObj {
                    return new_error(format!(
                        "argument to `rest` must be ArrayObj, got {:?}",
                        args.get(0).unwrap().object_type()
                    ))
                };

                let arr = match args.get(0) {
                    Some(Object::ArrayObj(inner)) => inner,
                    _ => panic!()
                };

                let length = arr.elements.len();

                if length > 0 {
                    let mut new_elements = vec![Object::NullObj(NullObj{}); length-1];
                    new_elements.clone_from_slice(&arr.elements[1..length]);
                    return Some(Object::ArrayObj(ArrayObj{ elements: new_elements }))
                }

                None
            }
        }
    ),
    Builtin::new(
        "push",
        BuiltinObj{
            func: |args| {
                if args.len() != 2 {
                    return new_error(format!(
                        "wrong number of arguments. got={}, want=2",
                        args.len()
                    ))
                };

                if args.get(0).unwrap().object_type() != ObjectType::ArrayObj {
                    return new_error(format!(
                        "argument to `push` must be ArrayObj, got {:?}",
                        args.get(0).unwrap().object_type()
                    ))
                };

                let arr = match args.get(0) {
                    Some(Object::ArrayObj(inner)) => inner,
                    _ => panic!()
                };

                let length = arr.elements.len();

                let mut new_elements = vec![Object::NullObj(NullObj{}); length];
                new_elements.clone_from_slice(&arr.elements);
                new_elements.push(args.get(1).cloned().unwrap());
                Some(Object::ArrayObj(ArrayObj{ elements: new_elements }))
            }
        }
    ),
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