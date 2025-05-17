use std::collections::HashMap;
use std::convert::Into;

struct SymbolScope(String);

const GLOBAL_SCOPE: SymbolScope = SymbolScope("GLOBAL".into());

struct Symbol {
    name: String,
    scope: SymbolScope,
    index: usize,
}

struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_definitions: usize,
}

impl SymbolTable {
    fn new() -> Self {
        Self { store: Default::default(), num_definitions: 0 }
    }
}