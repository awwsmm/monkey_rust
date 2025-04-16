enum ObjectType {
    IntegerObj,
    BooleanObj,
    NullObj,
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

struct Null{}

impl Object for Null {
    fn object_type() -> ObjectType {
        ObjectType::NullObj
    }

    fn inspect(&self) -> String {
        String::from("null")
    }
}