use crate::error::ReportError;
use crate::types::Type;
use cranelift_module::{FuncId, DataId};
use std::collections::HashMap;
use cranelift::prelude::{Value, Variable, EntityRef};
use std::cell::Cell;

pub struct Callables<'a> {
    parent: Option<Box<&'a Callables<'a>>>,
    values: HashMap<&'a str, Callable>,
    var_index: Option<Cell<usize>>,
}

#[derive(Debug)]
pub enum Callable {
    Arg(Type, Value),
    Var(Type, Variable, bool),
    Global(Type, DataId, bool),
    Func(Option<Type>, Vec<Type>, FuncId),
}

impl<'a> Callables<'a> {
    pub fn new() -> Callables<'a> {
        Callables {
            parent: None,
            values: HashMap::new(),
            var_index: Some(Cell::new(0)),
        }
    }

    pub fn create_child(&self) -> Callables {
        Callables {
            parent: Some(Box::new(self)),
            values: HashMap::new(),
            var_index: None
        }
    }

    fn get_var_index(&self) -> &Cell<usize> {
        match &self.var_index {
            None => match &self.parent {
                Some(parent) => parent.get_var_index(),
                None => unreachable!()
            }
            Some(var_index) => var_index,
        }
    }

    pub fn new_variable(&mut self) -> Variable {
        let var_index = self.get_var_index();
        let var = Variable::new(var_index.get());
        var_index.set(var_index.get() + 1);
        var
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
