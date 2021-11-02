use std::alloc::{alloc, dealloc, Layout};
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::mem::size_of;

use crate::as_string_bytes::AsStringBytes;

#[repr(C)]
pub struct Chars {
    ref_count: i64,
    pub length: usize,
}

impl Chars {
    pub fn empty() -> Chars {
        Chars {
            ref_count: -1,
            length: 0,
        }
    }

    pub fn from_bytes(bytes: &[u8]) -> Box<Chars> {
        unsafe {
            let (chars, offset) = Chars::alloc(0, bytes.len());
            let contents_ptr = chars.add(offset) as *mut u8;
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), contents_ptr, bytes.len());
            Box::from_raw(chars as *mut Chars)
        }
    }

    unsafe fn alloc(ref_count: i64, length: usize) -> (*mut u8, usize) {
        let ref_count_layout = Layout::new::<i64>();
        let length_layout = Layout::new::<usize>();
        let contents_layout = Layout::array::<u8>(length).unwrap();

        let layout = Layout::from_size_align(0, 1).unwrap();
        let (layout, ref_count_offset) = layout.extend(ref_count_layout).unwrap();
        let (layout, length_offset) = layout.extend(length_layout).unwrap();
        let (layout, contents_offset) = layout.extend(contents_layout).unwrap();
        let layout = layout.pad_to_align();

        let chars = alloc(layout);
        (chars.add(ref_count_offset) as *mut i64).write(ref_count);
        (chars.add(length_offset) as *mut usize).write(length);

        (chars, contents_offset)
    }

    pub fn append(self, value: impl AsStringBytes) -> Box<Chars> {
        let current_bytes = self.as_slice();
        let new_bytes = value.as_bytes();
        unsafe {
            let (chars, offset) = Chars::alloc(0, current_bytes.len() + new_bytes.len());
            let contents_ptr = chars.add(offset);
            std::ptr::copy_nonoverlapping(
                current_bytes.as_ptr(),
                contents_ptr,
                current_bytes.len(),
            );
            let contents_ptr = contents_ptr.add(current_bytes.len());
            std::ptr::copy_nonoverlapping(new_bytes.as_ptr(), contents_ptr, new_bytes.len());
            Box::from_raw(chars as *mut Chars)
        }
    }

    fn contents_ptr(&self) -> *const u8 {
        unsafe { (self as *const Chars as *const u8).add(size_of::<i64>() + size_of::<usize>()) }
    }

    pub fn as_slice(&self) -> &[u8] {
        &unsafe { std::slice::from_raw_parts(self.contents_ptr(), self.length) }
    }

    unsafe fn drop_slice(&mut self) {
        dealloc(
            self.contents_ptr() as *mut u8,
            Layout::array::<u8>(self.length).unwrap(),
        );
    }
}

impl Drop for Chars {
    fn drop(&mut self) {
        if self.length > 0 {
            // unsafe { self.drop_slice() }
        }
    }
}

impl Display for Chars {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", unsafe {
            std::str::from_utf8_unchecked(self.as_slice())
        })?;
        Ok(())
    }
}

impl AsStringBytes for Chars {
    fn as_bytes(&self) -> &[u8] {
        self.as_slice()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constructs_a_chars() {
        let chars = Chars::from_bytes("test".as_bytes());

        assert_eq!(chars.length, "test".len());
        assert_eq!(chars.as_slice(), "test".as_bytes());
    }
}
