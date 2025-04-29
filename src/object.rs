use crate::ast;
use std::cmp::PartialEq;

pub(crate) mod environment;

#[derive(PartialEq, Debug)]
pub(crate) enum ObjectType {
    IntegerObj,
    BooleanObj,
    NullObj,
    ReturnValueObj,
    ErrorObj,
    FunctionObj,
    StringObj,
    BuiltinObj,
    ArrayObj,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Object {
    IntegerObj(IntegerObj),
    BooleanObj(BooleanObj),
    NullObj(NullObj),
    ReturnValueObj(ReturnValueObj),
    ErrorObj(ErrorObj),
    FunctionObj(FunctionObj),
    StringObj(StringObj),
    BuiltinObj(BuiltinObj),
    ArrayObj(ArrayObj),
}

pub(crate) trait ObjectLike {
    fn object_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

impl Object {
    pub(crate) fn inner(&self) -> Box<&dyn ObjectLike> {
        match self {
            Object::IntegerObj(inner) => Box::new(inner),
            Object::BooleanObj(inner) => Box::new(inner),
            Object::NullObj(inner) => Box::new(inner),
            Object::ReturnValueObj(inner) => Box::new(inner),
            Object::ErrorObj(inner) => Box::new(inner),
            Object::FunctionObj(inner) => Box::new(inner),
            Object::StringObj(inner) => Box::new(inner),
            Object::BuiltinObj(inner) => Box::new(inner),
            Object::ArrayObj(inner) => Box::new(inner),
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

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct IntegerObj {
    pub(crate) value: i32,
}

impl ObjectLike for IntegerObj {
    fn object_type(&self) -> ObjectType {
        ObjectType::IntegerObj
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct BooleanObj {
    pub(crate) value: bool,
}

impl ObjectLike for BooleanObj {
    fn object_type(&self) -> ObjectType {
        ObjectType::BooleanObj
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct NullObj {}

impl ObjectLike for NullObj {
    fn object_type(&self) -> ObjectType {
        ObjectType::NullObj
    }

    fn inspect(&self) -> String {
        String::from("null")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct ReturnValueObj {
    pub(crate) value: Option<Box<Object>>
}

impl ObjectLike for ReturnValueObj {
    fn object_type(&self) -> ObjectType {
        ObjectType::ReturnValueObj
    }

    fn inspect(&self) -> String {
        self.value.as_ref().unwrap().inspect()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct ErrorObj {
    pub(crate) message: String
}

impl ObjectLike for ErrorObj {
    fn object_type(&self) -> ObjectType {
        ObjectType::ErrorObj
    }

    fn inspect(&self) -> String {
        format!("ERROR: {}", self.message)
    }
}

impl ErrorObj {
    pub(crate) fn new(message: String) -> Option<Object> {
        Some(Object::ErrorObj(Self { message }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct FunctionObj {
    pub(crate) parameters: Vec<ast::Identifier>,
    pub(crate) body: ast::BlockStatement,
    pub(crate) env: environment::Environment,
}

impl ObjectLike for FunctionObj {
    fn object_type(&self) -> ObjectType {
        ObjectType::FunctionObj
    }

    fn inspect(&self) -> String {
        let mut params = vec![];

        for p in self.parameters.iter() {
            params.push(p.to_string())
        }

        let params = params.join(", ");
        let body = self.body.to_string();

        format!("fn({}) {{\n{}\n}}", params, body)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct StringObj {
    pub(crate) value: String,
}

impl ObjectLike for StringObj {
    fn object_type(&self) -> ObjectType {
        ObjectType::StringObj
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

type BuiltinFunction = fn(Vec<Object>) -> Object;

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct BuiltinObj {
    pub(crate) func: BuiltinFunction,
}

impl ObjectLike for BuiltinObj {
    fn object_type(&self) -> ObjectType {
        ObjectType::BuiltinObj
    }

    fn inspect(&self) -> String {
        String::from("builtin function")
    }
}

#[derive(Debug, PartialEq, Clone)]
struct ArrayObj {
    elements: Vec<Object>,
}

impl ObjectLike for ArrayObj {
    fn object_type(&self) -> ObjectType {
        ObjectType::ArrayObj
    }

    fn inspect(&self) -> String {
        let mut elements = vec![];

        for e in self.elements.iter() {
            elements.push(e.inspect())
        }

        format!("[{}]", elements.join(", "))
    }
}