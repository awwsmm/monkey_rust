pub(crate) mod environment;

#[derive(PartialEq, Debug)]
pub(crate) enum ObjectType {
    IntegerObj,
    BooleanObj,
    NullObj,
    ReturnValueObj,
    ErrorObj,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
    ReturnValue(ReturnValue),
    Error(Error),
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
            Object::ReturnValue(inner) => Box::new(inner),
            Object::Error(inner) => Box::new(inner),
        }
    }
}

pub(crate) trait IsError {
    fn is_error(&self) -> bool;
}

impl IsError for Option<Object> {
    fn is_error(&self) -> bool {
        match self {
            Some(object) => object.inner().object_type() == ObjectType::ErrorObj,
            _ => false,
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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub(crate) struct Boolean {
    pub(crate) value: bool,
}

impl ObjectLike for Boolean {
    fn object_type(&self) -> ObjectType {
        ObjectType::BooleanObj
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Null{}

impl ObjectLike for Null {
    fn object_type(&self) -> ObjectType {
        ObjectType::NullObj
    }

    fn inspect(&self) -> String {
        String::from("null")
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct ReturnValue {
    pub(crate) value: Option<Box<Object>>
}

impl ObjectLike for ReturnValue {
    fn object_type(&self) -> ObjectType {
        ObjectType::ReturnValueObj
    }

    fn inspect(&self) -> String {
        self.value.as_ref().unwrap().inspect()
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Error {
    pub(crate) message: String
}

impl ObjectLike for Error {
    fn object_type(&self) -> ObjectType {
        ObjectType::ErrorObj
    }

    fn inspect(&self) -> String {
        format!("ERROR: {}", self.message)
    }
}

impl Error {
    pub(crate) fn new(message: String) -> Option<Object> {
        Some(Object::Error(Self { message }))
    }
}