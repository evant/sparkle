use std::error::Error;
use std::fmt;
use std::fmt::Formatter;

use nom::error::ErrorKind;

use crate::error::ReportError::{ReadError, SendError};

#[derive(Debug)]
pub enum ReportError<'a> {
    ReadError(nom::Err<(&'a str, ErrorKind)>),
    SendError(String),
}

impl fmt::Display for ReportError<'_> {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        unimplemented!()
    }
}

impl Error for ReportError<'_> {}

impl<'a> From<nom::Err<(&'a str, ErrorKind)>> for ReportError<'a> {
    fn from(e: nom::Err<(&'a str, ErrorKind)>) -> Self {
        ReadError(e)
    }
}

impl From<target_lexicon::ParseError> for ReportError<'_> {
    fn from(e: target_lexicon::ParseError) -> Self {
        SendError(e.to_string())
    }
}

impl From<cranelift::prelude::isa::LookupError> for ReportError<'_> {
    fn from(e: cranelift::prelude::isa::LookupError) -> Self {
        SendError(e.to_string())
    }
}

impl From<cranelift_module::ModuleError> for ReportError<'_> {
    fn from(e: cranelift_module::ModuleError) -> Self {
        SendError(e.to_string())
    }
}
