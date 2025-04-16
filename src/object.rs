enum ObjectType {
    IntegerObj,
    BooleanObj,
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

struct Boolean {
    value: bool,
}

impl Object for Boolean {
    fn object_type() -> ObjectType {
        ObjectType::BooleanObj
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}