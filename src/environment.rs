use crate::object::Object;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Environment {
    bindings: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        self.bindings.get(name)
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.bindings.insert(name.to_string(), value);
    }
}
