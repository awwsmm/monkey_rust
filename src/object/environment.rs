use crate::object;
use std::collections::HashMap;

pub(crate) struct Environment {
    store: HashMap<String, object::Object>
}

impl Environment {
    pub(crate) fn new() -> Self {
        Self { store: Default::default() }
    }

    pub(crate) fn get(&self, name: &str) -> Option<object::Object> {
        self.store.get(name).cloned()
    }

    pub(crate) fn set(&mut self, name: &str, val: object::Object) -> Option<object::Object> {
        self.store.insert(name.to_owned(), val.clone());
        Some(val)
    }
}