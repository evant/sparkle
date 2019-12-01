use crate::error::ReportError;
use crate::types::Type;
use cranelift::codegen::ir::StackSlot;
use cranelift_module::FuncId;
use std::collections::HashMap;
use cranelift::prelude::Value;

pub struct Callables<'a> {
    parent: Option<Box<&'a Callables<'a>>>,
    values: HashMap<&'a str, Callable>,
}

pub enum Callable {
    Arg(Type, Value),
    Var(Type, StackSlot, bool),
    Func(Option<Type>, Vec<Type>, FuncId),
}

impl<'a> Callables<'a> {
    pub fn new() -> Callables<'a> {
        Callables {
            parent: None,
            values: HashMap::new(),
        }
    }

    pub fn create_child(&self) -> Callables {
        Callables {
            parent: Some(Box::new(self)),
            values: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: &'a str, value: Callable) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Result<&Callable, ReportError> {
        match self.values.get(name) {
            None => match &self.parent {
                None => Err(ReportError::LookupError(name.to_owned())),
                Some(p) => p.get(name),
            },
            Some(value) => Ok(value),
        }
    }
}
