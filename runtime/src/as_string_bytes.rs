pub trait AsStringBytes {
    fn as_bytes(&self) -> Option<&[u8]>;
}

impl AsStringBytes for bool {
    fn as_bytes(&self) -> Option<&[u8]> {
        Some(if *self { "yes" } else { "no" }.as_bytes())
    }
}

impl AsStringBytes for f64 {
    fn as_bytes(&self) -> Option<&[u8]> {
        todo!()
    }
}
