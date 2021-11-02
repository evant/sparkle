pub trait AsStringBytes {
    fn as_bytes(&self) -> &[u8];
}

impl AsStringBytes for bool {
    fn as_bytes(&self) -> &[u8] {
        if *self { "yes" } else { "no" }.as_bytes()
    }
}

impl AsStringBytes for f64 {
    fn as_bytes(&self) -> &[u8] {
        todo!()
    }
}
