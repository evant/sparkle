use crate::error::ReportError;
use crate::types::Type;
use cranelift::codegen::ir::StackSlot;
use cranelift_module::FuncId;
use std::collections::HashMap;

pub struct Callables<'a> {
    values: HashMap<&'a str, Callable>,
}

#[derive(Copy, Clone)]
pub enum Callable {
    Var(Type, StackSlot, bool),
    Func(Type, FuncId),
}

impl<'a> Callables<'a> {

    pub fn new() -> Callables<'a> {
        Callables {
            values: HashMap::new()
        }
    }

    pub fn insert(&mut self, name: &'a str, value: Callable) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Result<&Callable, ReportError> {
        self.values
            .get(name)
            .ok_or_else(|| ReportError::LookupError(name.to_owned()))
    }
}
