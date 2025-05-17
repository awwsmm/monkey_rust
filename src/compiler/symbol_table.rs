use std::collections::HashMap;
use std::convert::Into;

#[derive(Debug, Clone, PartialEq)]
struct SymbolScope(&'static str);

const GLOBAL_SCOPE: SymbolScope = SymbolScope("GLOBAL");

#[derive(Debug, Clone, PartialEq)]
struct Symbol {
    name: String,
    scope: SymbolScope,
    index: usize,
}

impl Symbol {
    fn new(name: impl Into<String>, scope: SymbolScope, index: usize) -> Self {
        Self { name: name.into(), scope, index, }
    }
}

struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_definitions: usize,
}

impl SymbolTable {
    fn new() -> Self {
        Self { store: Default::default(), num_definitions: 0 }
    }

    fn define(&mut self, name: impl Into<String>) -> Symbol {
        let name = name.into();
        let symbol = Symbol::new(name.clone(), GLOBAL_SCOPE, self.num_definitions);
        self.store.insert(name, symbol.clone());
        self.num_definitions += 1;
        symbol
    }

    fn resolve(&self, name: impl Into<String>) -> Option<Symbol> {
        let name = name.into();
        self.store.get(&name).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_define() {
        let mut expected = HashMap::new();
        expected.insert("a", Symbol::new("a", GLOBAL_SCOPE, 0));
        expected.insert("b", Symbol::new("b", GLOBAL_SCOPE, 1));

        let mut global = SymbolTable::new();

        let mut should_panic = false;

        let a = global.define("a");
        if a != expected["a"] {
            should_panic = true;
            eprintln!("expected a={:?}, got={:?}", expected["a"], a)
        }

        let b = global.define("b");
        if b != expected["b"] {
            should_panic = true;
            eprintln!("expected b={:?}, got={:?}", expected["b"], b)
        }
    }

    #[test]
    fn test_resolve_global() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        let expected = vec![
            Symbol::new("a", GLOBAL_SCOPE, 0),
            Symbol::new("b", GLOBAL_SCOPE, 1),
        ];

        let mut should_panic = false;

        for sym in expected.into_iter() {
            let result = match global.resolve(sym.name.clone()) {
                None => {
                    should_panic = true;
                    eprintln!("name {} not resolvable", sym.name);
                    continue
                }
                Some(result) => result
            };
            if result != sym {
                should_panic = true;
                eprintln!("expected {} to resolve to {:?}, got={:?}",
                    sym.name, sym, result)
            }
        }

    }
}