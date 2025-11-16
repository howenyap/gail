use crate::object::Object;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Environment {
    bindings: HashMap<String, Object>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            outer: None,
        }
    }

    pub fn extended_with(self, new_bindings: HashMap<String, Object>) -> Self {
        Self {
            bindings: new_bindings,
            outer: Some(Box::new(self)),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        self.bindings
            .get(name)
            .or_else(|| self.outer.as_ref().and_then(|outer| outer.get(name)))
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.bindings.insert(name.to_string(), value);
    }
}
