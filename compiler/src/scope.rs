use std::collections::HashMap;

// Generic scope stack
pub struct ScopeStack<T> {
    stack: Vec<HashMap<String, T>>,
}

impl<T> ScopeStack<T> {
    pub fn new() -> Self {
        Self {
            stack: vec![HashMap::new()],
        }
    }

    pub fn push(&mut self) {
        self.stack.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.stack.pop().expect("Cannot pop from empty scope stack");
    }

    pub fn insert(&mut self, name: String, value: T) -> Result<(), String>
    where
        T: Clone,
    {
        let current_scope = self.stack.last_mut().unwrap();
        if current_scope.contains_key(&name) {
            return Err(format!(
                "Variable '{}' already declared in this scope",
                name
            ));
        }
        current_scope.insert(name, value);
        Ok(())
    }

    pub fn lookup(&self, name: &str) -> Option<T>
    where
        T: Clone,
    {
        for scope in self.stack.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Some(val.clone());
            }
        }
        None
    }
}
