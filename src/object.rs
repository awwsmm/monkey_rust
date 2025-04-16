enum ObjectType {
    IntegerObj,
    BooleanObj,
    NullObj,
}

#[derive(Debug)]
pub(crate) enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
}

pub(crate) trait ObjectLike {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

impl Object {
    pub(crate) fn inner(&self) -> Box<&dyn ObjectLike> {
        match self {
            Object::Integer(inner) => Box::new(inner),
            Object::Boolean(inner) => Box::new(inner),
            Object::Null(inner) => Box::new(inner),
        }
    }
}

impl ObjectLike for Object {
    fn object_type(&self) -> ObjectType {
        self.inner().object_type()
    }

    fn inspect(&self) -> String {
        self.inner().inspect()
    }
}

#[derive(Debug)]
pub(crate) struct Integer {
    pub(crate) value: i32,
}

impl ObjectLike for Integer {
    fn object_type(&self) -> ObjectType {
        ObjectType::IntegerObj
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug)]
struct Boolean {
    value: bool,
}

impl ObjectLike for Boolean {
    fn object_type(&self) -> ObjectType {
        ObjectType::BooleanObj
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug)]
struct Null{}

impl ObjectLike for Null {
    fn object_type(&self) -> ObjectType {
        ObjectType::NullObj
    }

    fn inspect(&self) -> String {
        String::from("null")
    }
}