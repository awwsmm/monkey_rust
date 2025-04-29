use crate::object::ObjectLike;
use crate::object;

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
            }) ,
            _ => None,
        }
    }
}