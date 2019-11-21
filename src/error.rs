use std::error::Error;
use std::fmt;
use std::fmt::{Debug, Formatter, Result};

use nom::error::{ErrorKind, VerboseError};

use crate::error::ReportError::{ReadError, SendError, TypeError};

pub enum ReportError {
    ReadError(String),
    SendError(String),
    TypeError(String),
}

impl fmt::Display for ReportError {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            ReadError(e) => write!(f, "{}", e),
            SendError(e) => write!(f, "{}", e),
            TypeError(e) => write!(f, "{}", e),
        }
    }
}

impl fmt::Debug for ReportError {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            ReadError(e) => write!(f, "{}", e),
            SendError(e) => write!(f, "{}", e),
            TypeError(e) => write!(f, "{}", e),
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
        SendError(e.to_string())
    }
}

impl From<fmt::Error> for ReportError {
    fn from(e: fmt::Error) -> Self {
        SendError(e.to_string())
    }
}
