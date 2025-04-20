use crate::object;
use std::collections::HashMap;

struct Environment {
    store: HashMap<String, object::Object>
}

impl Environment {
    fn new() -> Self {
        Self { store: Default::default() }
    }

    fn get(&self, name: &str) -> Option<&object::Object> {
        self.store.get(name)
    }

    fn set(&mut self, name: &str, val: object::Object) {
        self.store.insert(name.to_owned(), val);
    }
}