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
}
