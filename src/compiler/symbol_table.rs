use std::collections::HashMap;
use std::convert::Into;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct SymbolScope(&'static str);

pub(crate) const LOCAL_SCOPE: SymbolScope = SymbolScope("LOCAL");
pub(crate) const GLOBAL_SCOPE: SymbolScope = SymbolScope("GLOBAL");
pub(crate) const BUILTIN_SCOPE: SymbolScope = SymbolScope("BUILTIN");
pub(crate) const FREE_SCOPE: SymbolScope = SymbolScope("FREE");

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Symbol {
    name: String,
    pub(crate) scope: SymbolScope,
    pub(crate) index: usize,
}

impl Symbol {
    fn new(name: impl Into<String>, scope: SymbolScope, index: usize) -> Self {
        Self { name: name.into(), scope, index, }
    }
}

#[derive(Clone, PartialEq)]
pub(crate) struct SymbolTable {
    pub(crate) outer: Option<Box<SymbolTable>>,

    store: HashMap<String, Symbol>,
    pub(crate) num_definitions: usize,

    pub(crate) free_symbols: Vec<Symbol>,
}

impl SymbolTable {
    pub(crate) fn new() -> Self {
        Self { outer: None, store: Default::default(), num_definitions: 0, free_symbols: Default::default() }
    }

    pub(crate) fn new_enclosed(outer: Box<SymbolTable>) -> Self {
        Self { outer: Some(outer), store: Default::default(), num_definitions: 0, free_symbols: Default::default() }
    }

    pub(crate) fn define(&mut self, name: impl Into<String>) -> Symbol {
        let name = name.into();

        let scope = if self.outer.is_none() {
            GLOBAL_SCOPE
        } else {
            LOCAL_SCOPE
        };

        let symbol = Symbol::new(name.clone(), scope, self.num_definitions);
        self.store.insert(name, symbol.clone());
        self.num_definitions += 1;
        symbol
    }

    pub(crate) fn resolve(&mut self, name: &str) -> Option<Symbol> {
        match self.store.get(name).cloned() {
            None => {
                match self.outer.as_mut() {
                    Some(mut outer) => {
                        match outer.resolve(name) {
                            None => None,
                            Some(obj) => {
                                if obj.scope == GLOBAL_SCOPE || obj.scope == BUILTIN_SCOPE {
                                    Some(obj)
                                } else {
                                    let free = self.define_free(obj);
                                    Some(free)
                                }
                            }
                        }
                    }
                    None => None,
                }
            },
            Some(obj) => Some(obj)
        }
    }

    pub(crate) fn define_builtin(&mut self, index: usize, name: &str) -> Symbol {
        let symbol = Symbol::new(name, BUILTIN_SCOPE, index);
        self.store.insert(String::from(name), symbol.clone());
        symbol
    }

    fn define_free(&mut self, original: Symbol) -> Symbol {
        let original_name = original.name.clone();
        self.free_symbols.push(original);

        let symbol = Symbol::new(original_name.as_str(), FREE_SCOPE, self.free_symbols.len() - 1);

        self.store.insert(original_name, symbol.clone());
        symbol
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

        let mut first_local: SymbolTable = SymbolTable::new_enclosed(Box::new(global));

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

        let mut second_local: SymbolTable = SymbolTable::new_enclosed(Box::new(first_local));

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
        let mut global: SymbolTable = SymbolTable::new();

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

        let mut local: SymbolTable = SymbolTable::new_enclosed(Box::new(global));

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
            let result = match local.resolve(sym.name.as_str()) {
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

        let mut first_local: SymbolTable = SymbolTable::new_enclosed(Box::new(global));

        first_local.define("c");
        first_local.define("d");

        let mut second_local: SymbolTable = SymbolTable::new_enclosed(Box::new(first_local.clone()));

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

        for mut tt in tests.into_iter() {
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

    #[test]
    fn test_define_resolve_builtins() {
        let mut global: SymbolTable = SymbolTable::new();

        let expected = vec![
            Symbol::new("a", BUILTIN_SCOPE, 0),
            Symbol::new("c", BUILTIN_SCOPE, 1),
            Symbol::new("e", BUILTIN_SCOPE, 2),
            Symbol::new("f", BUILTIN_SCOPE, 3),
        ];

        for (i, v) in expected.iter().enumerate() {
            global.define_builtin(i, v.name.as_str());
        }

        let first_local: SymbolTable = SymbolTable::new_enclosed(Box::new(global.clone()));
        let second_local: SymbolTable = SymbolTable::new_enclosed(Box::new(first_local.clone()));

        let mut should_panic = false;

        for mut table in vec![global, first_local, second_local] {
            for sym in expected.iter() {
                let result = match table.resolve(sym.name.as_str()) {
                    None => {
                        should_panic = true;
                        eprintln!("name {} not resolvable", sym.name);
                        continue
                    }
                    Some(result) => result
                };
                if result != *sym {
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

    #[test]
    fn test_resolve_free() {
        let mut global: SymbolTable = SymbolTable::new();

        global.define("a");
        global.define("b");

        let mut first_local: SymbolTable = SymbolTable::new_enclosed(Box::new(global));

        first_local.define("c");
        first_local.define("d");

        let mut second_local: SymbolTable = SymbolTable::new_enclosed(Box::new(first_local.clone()));

        second_local.define("e");
        second_local.define("f");

        struct Test {
            table: SymbolTable,
            expected_symbols: Vec<Symbol>,
            expected_free_symbols: Vec<Symbol>,
        }

        impl Test {
            fn new(table: SymbolTable, expected_symbols: Vec<Symbol>, expected_free_symbols: Vec<Symbol>) -> Self {
                Self { table, expected_symbols, expected_free_symbols }
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
                ],
                vec![],
            ),
            Test::new(
                second_local,
                vec![
                    Symbol::new("a", GLOBAL_SCOPE, 0),
                    Symbol::new("b", GLOBAL_SCOPE, 1),
                    Symbol::new("c", FREE_SCOPE, 0),
                    Symbol::new("d", FREE_SCOPE, 1),
                    Symbol::new("e", LOCAL_SCOPE, 0),
                    Symbol::new("f", LOCAL_SCOPE, 1),
                ],
                vec![
                    Symbol::new("c", LOCAL_SCOPE, 0),
                    Symbol::new("d", LOCAL_SCOPE, 1),
                ],
            ),
        ];

        let mut should_panic = false;

        for mut tt in tests.into_iter() {
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

            if tt.table.free_symbols.len() != tt.expected_free_symbols.len() {
                should_panic = true;
                eprintln!("wrong number of free symbols. got={}, want={}",
                          tt.table.free_symbols.len(), tt.expected_free_symbols.len());
                continue
            }

            for (i, sym) in tt.expected_free_symbols.into_iter().enumerate() {
                let result = tt.table.free_symbols[i].clone();
                if result != sym {
                    should_panic = true;
                    eprintln!("wrong free symbol. got={:?}, want={:?}",
                              result, sym);
                }
            }
        }

        if should_panic {
            panic!()
        }
    }

    #[test]
    fn test_resolve_unresolvable_free() {
        let mut global: SymbolTable = SymbolTable::new();

        global.define("a");

        let mut first_local: SymbolTable = SymbolTable::new_enclosed(Box::new(global));

        first_local.define("c");

        let mut second_local: SymbolTable = SymbolTable::new_enclosed(Box::new(first_local.clone()));

        second_local.define("e");
        second_local.define("f");

        let expected = vec![
            Symbol::new("a", GLOBAL_SCOPE, 0),
            Symbol::new("c", FREE_SCOPE, 0),
            Symbol::new("e", LOCAL_SCOPE, 0),
            Symbol::new("f", LOCAL_SCOPE, 1),
        ];

        let mut should_panic = false;

        for sym in expected.into_iter() {
            let result = match second_local.resolve(sym.name.as_str()) {
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

        let expected_unresolvable = vec![
            "b",
            "d",
        ];

        for name in expected_unresolvable.into_iter() {
            if let Some(_) = second_local.resolve(name) {
                should_panic = true;
                eprintln!("name {} resolved, but was expected not to", name);
            }
        }

        if should_panic {
            panic!()
        }
    }
}