enum ObjectType {
    IntegerObj
}

trait Object {
    fn object_type() -> ObjectType;
    fn inspect(&self) -> String;
}

struct Integer {
    value: i32,
}

impl Object for Integer {
    fn object_type() -> ObjectType {
        ObjectType::IntegerObj
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}