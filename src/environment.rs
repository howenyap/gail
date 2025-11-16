use crate::object::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Env {
    frame: Rc<Frame>,
}

#[derive(Debug)]
struct Frame {
    bindings: RefCell<HashMap<String, Object>>,
    outer: Option<Env>,
}

impl Frame {
    pub fn new(outer: Option<Env>) -> Self {
        Self {
            bindings: RefCell::new(HashMap::new()),
            outer,
        }
    }
}

impl Env {
    pub fn new() -> Self {
        let frame = Frame::new(None);
        Self {
            frame: Rc::new(frame),
        }
    }

    pub fn set(&self, name: &str, value: Object) {
        self.frame
            .bindings
            .borrow_mut()
            .insert(name.to_string(), value);
    }

    pub fn extend(&self, bindings: HashMap<String, Object>) -> Self {
        let frame = Frame::new(Some(self.clone()));
        let inner = Self {
            frame: Rc::new(frame),
        };

        inner.frame.bindings.borrow_mut().extend(bindings);
        inner
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        if let Some(v) = self.frame.bindings.borrow().get(name) {
            Some(v.clone())
        } else {
            self.frame.outer.as_ref().and_then(|outer| outer.get(name))
        }
    }
}
