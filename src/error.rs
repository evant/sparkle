use std::error::Error;
use std::fmt;
use std::fmt::{Formatter, Result};

use crate::error::ReportError::{ModuleError, ReadError, SendError, TypeError, LookupError};

pub enum ReportError {
    ReadError(String),
    SendError(String),
    ModuleError(cranelift_module::ModuleError),
    TypeError(String),
    LookupError(String),
}

impl fmt::Display for ReportError {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            ReadError(e) => write!(f, "{}", e),
            SendError(e) => write!(f, "{}", e),
            ModuleError(e) => fmt::Display::fmt(e, f),
            TypeError(e) => write!(f, "{}", e),
            LookupError(e) => write!(f, "I didn't know '{}'", e),
        }
    }
}

impl fmt::Debug for ReportError {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            ReadError(e) => write!(f, "{}", e),
            SendError(e) => write!(f, "{}", e),
            ModuleError(e) => fmt::Debug::fmt(e, f),
            TypeError(e) => write!(f, "{}", e),
            LookupError(e) => write!(f, "I didn't know '{}'", e),
        }
    }
}

impl Error for ReportError {}

impl From<target_lexicon::ParseError> for ReportError {
    fn from(e: target_lexicon::ParseError) -> Self {
        SendError(e.to_string())
    }
}

impl From<cranelift::prelude::isa::LookupError> for ReportError {
    fn from(e: cranelift::prelude::isa::LookupError) -> Self {
        SendError(e.to_string())
    }
}

impl From<cranelift_module::ModuleError> for ReportError {
    fn from(e: cranelift_module::ModuleError) -> Self {
        ModuleError(e)
    }
}

impl From<fmt::Error> for ReportError {
    fn from(e: fmt::Error) -> Self {
        SendError(e.to_string())
    }
}
