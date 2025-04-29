use crate::evaluator::NULL;
use crate::object;
use crate::object::ObjectLike;

pub(crate) struct Builtin{}

impl Builtin {
    pub(crate) fn from(string: &str) -> Option<object::BuiltinObj> {
        match string {
            "len" => Some(object::BuiltinObj{
                func: |args| {
                    if args.len() != 1 {
                        return object::ErrorObj::new(format!(
                            "wrong number of arguments. got={}, want=1",
                            args.len()
                        )).unwrap()
                    }

                    match args.get(0) {
                        Some(object::Object::ArrayObj(arg)) =>
                            return object::Object::IntegerObj(object::IntegerObj{ value: arg.elements.len() as i32 }),

                        Some(object::Object::StringObj(arg)) =>
                            return object::Object::IntegerObj(object::IntegerObj{ value: arg.value.len() as i32 }),

                        _ =>
                            return object::ErrorObj::new(format!(
                                "argument to `len` not supported, got {:?}",
                                args.get(0).map(|x| x.object_type())
                            )).unwrap()
                    }
                }
            }),

            "first" => Some(object::BuiltinObj{
                func: |args| {
                    if args.len() != 1 {
                        return object::ErrorObj::new(format!(
                            "wrong number of arguments. got={}, want=1",
                            args.len()
                        )).unwrap()
                    };

                    if args.get(0).unwrap().object_type() != object::ObjectType::ArrayObj {
                        return object::ErrorObj::new(format!(
                            "argument to `first` must be ArrayObj, got {:?}",
                            args.get(0).unwrap().object_type()
                        )).unwrap()
                    };

                    let arr = match args.get(0) {
                        Some(object::Object::ArrayObj(inner)) => inner,
                        _ => panic!()
                    };

                    if arr.elements.len() > 0 {
                        return arr.elements.get(0).cloned().unwrap()
                    }

                    NULL.unwrap()
                }
            }),

            _ => None,
        }
    }
}