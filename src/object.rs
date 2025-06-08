use crate::{ast, code};
use std::cell::RefCell;
use std::cmp::PartialEq;
use std::collections::BTreeMap;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::rc::Rc;

pub(crate) mod environment;
pub(crate) mod builtins;

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
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
    HashObj,
    CompiledFunctionObj,
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
    HashObj(HashObj),
    CompiledFunctionObj(CompiledFunctionObj),
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
            Object::HashObj(inner) => Box::new(inner),
            Object::CompiledFunctionObj(inner) => Box::new(inner),
        }
    }

    pub(crate) fn as_hashable(&self) -> Option<Hashable> {
        match self {
            Object::IntegerObj(inner) => Some(Hashable::IntegerObj(inner.clone())),
            Object::BooleanObj(inner) => Some(Hashable::BooleanObj(inner.clone())),
            Object::StringObj(inner) => Some(Hashable::StringObj(inner.clone())),
            _ => None,
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

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
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

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
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
    pub(crate) env: Rc<RefCell<environment::Environment>>,
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

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
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
pub(crate) struct ArrayObj {
    pub(crate) elements: Vec<Object>,
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

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub(crate) struct HashKey {
    object_type: ObjectType,
    value: u64,
}

pub(crate) trait HasHashKey {
    fn hash_key(&self) -> HashKey;
}

impl HasHashKey for BooleanObj {
    fn hash_key(&self) -> HashKey {
        HashKey{ object_type: self.object_type(), value: if self.value { 1 } else { 0 }}
    }
}

impl HasHashKey for IntegerObj {
    fn hash_key(&self) -> HashKey {
        HashKey{ object_type: self.object_type(), value: self.value as u64 }
    }
}

impl HasHashKey for StringObj {
    fn hash_key(&self) -> HashKey {
        let mut s = DefaultHasher::new();
        self.value.hash(&mut s);

        HashKey{ object_type: self.object_type(), value: s.finish() }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct HashPair {
    pub(crate) key: Object,
    pub(crate) value: Object,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct HashObj {
    pub(crate) pairs: BTreeMap<HashKey, HashPair>
}

impl ObjectLike for HashObj {
    fn object_type(&self) -> ObjectType {
        ObjectType::HashObj
    }

    fn inspect(&self) -> String {
        let mut pairs = vec![];

        for (_, pair) in self.pairs.iter() {
            pairs.push(format!("{}: {}",
                pair.key.inspect(), pair.value.inspect()))
        }

        format!("{{{}}}", pairs.join(", "))
    }
}

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub(crate) enum Hashable {
    BooleanObj(BooleanObj),
    IntegerObj(IntegerObj),
    StringObj(StringObj),
}

impl HasHashKey for Hashable {
    fn hash_key(&self) -> HashKey {
        match self {
            Hashable::BooleanObj(inner) => inner.hash_key(),
            Hashable::IntegerObj(inner) => inner.hash_key(),
            Hashable::StringObj(inner) => inner.hash_key(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct CompiledFunctionObj {
    pub(crate) instructions: code::Instructions,
    pub(crate) num_locals: usize,
    pub(crate) num_parameters: usize,
}

impl ObjectLike for CompiledFunctionObj {
    fn object_type(&self) -> ObjectType {
        ObjectType::CompiledFunctionObj
    }

    fn inspect(&self) -> String {
        format!("CompiledFunction[{:p}]", &self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_hash_key() {
        let hello1 = StringObj{ value: "Hello World".to_string() };
        let hello2 = StringObj{ value: "Hello World".to_string() };
        let diff1 = StringObj{ value: "My name is johnny".to_string() };
        let diff2 = StringObj{ value: "My name is johnny".to_string() };

        let mut should_panic = false;

        if hello1.hash_key() != hello2.hash_key() {
            should_panic = true;
            eprintln!("strings with same content have different hash keys")
        }

        if diff1.hash_key() != diff2.hash_key() {
            should_panic = true;
            eprintln!("strings with same content have different hash keys")
        }

        if hello1.hash_key() == diff1.hash_key() {
            should_panic = true;
            eprintln!("strings with different content have same hash keys")
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_boolean_hash_key() {
        let hello1 = BooleanObj{ value: true };
        let hello2 = BooleanObj{ value: true };
        let diff1 = BooleanObj{ value: false };
        let diff2 = BooleanObj{ value: false };

        let mut should_panic = false;

        if hello1.hash_key() != hello2.hash_key() {
            should_panic = true;
            eprintln!("booleans with same value have different hash keys")
        }

        if diff1.hash_key() != diff2.hash_key() {
            should_panic = true;
            eprintln!("booleans with same value have different hash keys")
        }

        if hello1.hash_key() == diff1.hash_key() {
            should_panic = true;
            eprintln!("booleans with different values have same hash keys")
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_integer_hash_key() {
        let hello1 = IntegerObj{ value: 42 };
        let hello2 = IntegerObj{ value: 42 };
        let diff1 = IntegerObj{ value: 43 };
        let diff2 = IntegerObj{ value: 43 };

        let mut should_panic = false;

        if hello1.hash_key() != hello2.hash_key() {
            should_panic = true;
            eprintln!("integers with same v have different hash keys")
        }

        if diff1.hash_key() != diff2.hash_key() {
            should_panic = true;
            eprintln!("integers with same value have different hash keys")
        }

        if hello1.hash_key() == diff1.hash_key() {
            should_panic = true;
            eprintln!("integers with different values have same hash keys")
        }

        if should_panic {
            panic!()
        }
    }
}