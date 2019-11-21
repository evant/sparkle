#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    String,
    Number,
}

impl Type {
    pub fn is_number(&self) -> bool {
        *self == Type::Number
    }
}
