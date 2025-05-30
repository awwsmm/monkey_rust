use std::collections::HashMap;
use std::convert::Into;

#[derive(Debug, Clone, PartialEq)]
struct SymbolScope(&'static str);

const GLOBAL_SCOPE: SymbolScope = SymbolScope("GLOBAL");

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Symbol {
    name: String,
    scope: SymbolScope,
    pub(crate) index: usize,
}

impl Symbol {
    fn new(name: impl Into<String>, scope: SymbolScope, index: usize) -> Self {
        Self { name: name.into(), scope, index, }
    }
}

#[derive(Clone)]
pub(crate) struct SymbolTable {
    outer: Box<SymbolTable>,

    store: HashMap<String, Symbol>,
    num_definitions: usize,
}

impl SymbolTable {
    pub(crate) fn new() -> Self {
        Self { store: Default::default(), num_definitions: 0 }
    }

    pub(crate) fn define(&mut self, name: impl Into<String>) -> Symbol {
        let name = name.into();
        let symbol = Symbol::new(name.clone(), GLOBAL_SCOPE, self.num_definitions);
        self.store.insert(name, symbol.clone());
        self.num_definitions += 1;
        symbol
    }

    pub(crate) fn resolve(&self, name: &str) -> Option<Symbol> {
        self.store.get(name).cloned()
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
        expected.insert("c", Symbol::new("c", LOCAL_SCOPE, 0));
        expected.insert("d", Symbol::new("d", LOCAL_SCOPE, 1));
        expected.insert("e", Symbol::new("e", LOCAL_SCOPE, 0));
        expected.insert("f", Symbol::new("f", LOCAL_SCOPE, 1));

        let mut should_panic = false;

        let mut global: SymbolTable = SymbolTable::new();

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

        let mut first_local: SymbolTable = SymbolTable::new_enclosed(global);

        let c = first_local.define("c");
        if c != expected["c"] {
            should_panic = true;
            eprintln!("expected c={:?}, got={:?}", expected["c"], c)
        }

        let d = first_local.define("d");
        if d != expected["d"] {
            should_panic = true;
            eprintln!("expected d={:?}, got={:?}", expected["d"], d)
        }

        let mut second_local: SymbolTable = SymbolTable::new_enclosed(first_local);

        let e = second_local.define("e");
        if e != expected["e"] {
            should_panic = true;
            eprintln!("expected e={:?}, got={:?}", expected["e"], e)
        }

        let f = second_local.define("f");
        if f != expected["f"] {
            should_panic = true;
            eprintln!("expected f={:?}, got={:?}", expected["f"], f)
        }

        if should_panic {
            panic!()
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
            let result = match global.resolve(sym.name.as_str()) {
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

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_resolve_local() {
        let mut global: SymbolTable = SymbolTable::new();
        global.define("a");
        global.define("b");

        let mut local: SymbolTable = SymbolTable::new_enclosed(global);
        local.define("c");
        local.define("d");

        let expected = vec![
            Symbol::new("a", GLOBAL_SCOPE, 0),
            Symbol::new("b", GLOBAL_SCOPE, 1),
            Symbol::new("c", LOCAL_SCOPE, 0),
            Symbol::new("d", LOCAL_SCOPE, 1),
        ];

        let mut should_panic = false;

        for sym in expected.into_iter() {
            let result = match global.resolve(sym.name.as_str()) {
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

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_resolve_nested_local() {
        let mut global: SymbolTable = SymbolTable::new();
        global.define("a");
        global.define("b");

        let mut first_local: SymbolTable = SymbolTable::new_enclosed(global);
        first_local.define("c");
        first_local.define("d");

        let mut second_local: SymbolTable = SymbolTable::new_enclosed(first_local);
        second_local.define("e");
        second_local.define("f");

        struct Test {
            table: SymbolTable,
            expected_symbols: Vec<Symbol>,
        }

        impl Test {
            fn new(table: SymbolTable, expected_symbols: Vec<Symbol>) -> Self {
                Self { table, expected_symbols }
            }
        }

        let tests = vec![
            Test::new(
                first_local,
                vec![
                    Symbol::new("a", GLOBAL_SCOPE, 0),
                    Symbol::new("b", GLOBAL_SCOPE, 1),
                    Symbol::new("c", LOCAL_SCOPE, 0),
                    Symbol::new("d", LOCAL_SCOPE, 1),
                ]
            ),
            Test::new(
                second_local,
                vec![
                    Symbol::new("a", GLOBAL_SCOPE, 0),
                    Symbol::new("b", GLOBAL_SCOPE, 1),
                    Symbol::new("e", LOCAL_SCOPE, 0),
                    Symbol::new("f", LOCAL_SCOPE, 1),
                ]
            ),
        ];

        let mut should_panic = false;

        for tt in tests.into_iter() {
            for sym in tt.expected_symbols.into_iter() {
                let result = match tt.table.resolve(sym.name.as_str()) {
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

        if should_panic {
            panic!()
        }
    }
}