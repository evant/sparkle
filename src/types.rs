use crate::error::ReportError;
use crate::pst::Literal;
use nom::lib::std::convert::TryInto;
use std::fmt::{Error, Formatter};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Type {
    Chars,
    Number,
    Boolean,
    Array(ArrayType),
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ArrayType {
    Chars,
    Number,
    Boolean,
}

impl Type {
    pub fn is_number(self) -> bool {
        self == Type::Number
    }

    pub fn is_boolean(self) -> bool {
        self == Type::Boolean
    }

    pub fn check(self, expected: Type) -> Result<Type, ReportError> {
        self.type_check(expected)?;
        Ok(self)
    }

    pub fn check_bin<T>(
        self,
        left: Type,
        right: Type,
        f: impl FnOnce() -> T,
    ) -> Result<(Type, T), ReportError> {
        self.type_check(left)?;
        self.type_check(right)?;
        Ok((self, f()))
    }

    fn type_check(self, actual_type: crate::types::Type) -> Result<(), ReportError> {
        if self == actual_type {
            Ok(())
        } else {
            Err(ReportError::TypeError(format!(
                "expected {} but got {}",
                self, actual_type
            )))
        }
    }
}

impl Into<Type> for &Literal<'_> {
    fn into(self) -> Type {
        match self {
            Literal::Chars(_) => Type::Chars,
            Literal::Number(_) => Type::Number,
            Literal::Boolean(_) => Type::Boolean,
        }
    }
}

impl Into<Type> for ArrayType {
    fn into(self) -> Type {
        match self {
            ArrayType::Chars => Type::Chars,
            ArrayType::Number => Type::Number,
            ArrayType::Boolean => Type::Boolean,
        }
    }
}

impl TryInto<ArrayType> for Type {
    type Error = ReportError;

    fn try_into(self) -> Result<ArrayType, Self::Error> {
        Ok(match self {
            Type::Chars => ArrayType::Chars,
            Type::Number => ArrayType::Number,
            Type::Boolean => ArrayType::Boolean,
            Type::Array(_) => {
                return Err(ReportError::TypeError(
                    "cannot store arrays in arrays".to_string(),
                ))
            }
        })
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Type::Chars => write!(f, "string"),
            Type::Number => write!(f, "number"),
            Type::Boolean => write!(f, "boolean"),
            Type::Array(type_) => write!(f, "[{}]", type_),
        }
    }
}

impl std::fmt::Display for ArrayType {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            ArrayType::Chars => write!(f, "string"),
            ArrayType::Number => write!(f, "number"),
            ArrayType::Boolean => write!(f, "boolean"),
        }
    }
}
