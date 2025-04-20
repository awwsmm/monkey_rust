use crate::object;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct Environment {
    store: HashMap<String, object::Object>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub(crate) fn new(outer: Option<Box<Environment>>) -> Self {
        Self { store: Default::default(), outer }
    }

    pub(crate) fn get(&self, name: &str) -> Option<object::Object> {
        match self.store.get(name) {
            None => self.outer.as_ref()?.store.get(name).cloned(),
            Some(obj) => Some(obj.clone())
        }
    }

    pub(crate) fn set(&mut self, name: &str, val: object::Object) -> Option<object::Object> {
        self.store.insert(name.to_owned(), val.clone());
        Some(val)
    }
}