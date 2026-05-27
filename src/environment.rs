use std::collections::HashMap;

use crate::types::Type;

#[derive(Debug, Clone)]
pub enum Symbol {
    Variable {
        ty: Type,
    },
    Function {
        params: Vec<(String, Type)>,
        return_type: Type,
    },
}

pub struct Environment {
    symbols: HashMap<String, Symbol>,
    pub parent: Option<Box<Environment>>,
}

impl Environment {
    pub fn new(parent: Option<Box<Environment>>) -> Self {
        Self {
            symbols: HashMap::new(),
            parent: parent,
        }
    }

    pub fn define(&mut self, name: String, symbol: Symbol) -> Result<(), String> {
        if self.symbols.contains_key(&name) {
            return Err(format!(
                "Error: Identifier '{}' has already been declared in this scope.",
                name
            ));
        }
        self.symbols.insert(name, symbol);
        Ok(())
    }

    pub fn lookup(&self, name: &str) -> Option<Symbol> {
        if let Some(symbol) = self.symbols.get(name) {
            return Some(symbol.clone());
        }
        self.parent.as_ref().and_then(|p| p.lookup(name))
    }
}
