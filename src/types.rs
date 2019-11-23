use std::fmt::{Error, Formatter};

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    String,
    Number,
    Boolean,
}

impl Type {
    pub fn is_number(&self) -> bool {
        *self == Type::Number
    }

    pub fn is_boolean(&self) -> bool {
        *self == Type::Boolean
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Type::String => write!(f, "string"),
            Type::Number => write!(f, "number"),
            Type::Boolean => write!(f, "boolean"),
        }
    }
}
