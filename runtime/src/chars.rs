use alloc::alloc::{alloc, dealloc};


use core::alloc::Layout;
use core::cmp::Ordering;
use core::fmt;
use core::fmt::{Debug, Display, Formatter};
use core::marker::PhantomData;
use core::mem::{size_of};


use core::{ptr, slice};



use crate::as_string_bytes::AsStringBytes;
use crate::rc::{Rc, ReferenceCounted};


#[repr(C)]
pub struct Chars {
    ref_count: i64,
    length: usize,
    _phantom: PhantomData<[u8]>,
}

impl ReferenceCounted for Chars {
    fn inc_ref_count(&mut self) {
        let ref_count = &mut self.ref_count;
        if *ref_count >= 0 {
            *ref_count += 1;
        }
    }

    fn dec_ref_count(&mut self) -> bool {
        let ref_count = &mut self.ref_count;
        if *ref_count >= 0 {
            *ref_count -= 1;
            if *ref_count < 0 {
                return true;
            }
        }
        return false;
    }

    unsafe fn dealloc(&mut self) {
        Chars::dealloc(self);
    }
}

impl Chars {
    pub fn from_str(str: &str) -> Rc<Chars> {
        Chars::from_bytes(str.as_bytes())
    }

    pub fn from_bytes(bytes: &[u8]) -> Rc<Chars> {
        unsafe {
            let chars = Chars::alloc(bytes.len());
            ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                (*chars).contents_ptr_mut(),
                bytes.len(),
            );
            Rc::from_ptr(chars)
        }
    }

    pub fn append(&self, value: impl AsStringBytes) -> Rc<Chars> {
        let (current_bytes, new_bytes) = (self.as_slice(), value.as_bytes());
        unsafe {
            let chars = Chars::alloc(current_bytes.len() + new_bytes.len());
            let contents_ptr = (*chars).contents_ptr_mut();
            ptr::copy_nonoverlapping(current_bytes.as_ptr(), contents_ptr, current_bytes.len());
            let contents_ptr = contents_ptr.add(current_bytes.len());
            ptr::copy_nonoverlapping(new_bytes.as_ptr(), contents_ptr, new_bytes.len());
            Rc::from_ptr(chars)
        }
    }

    pub fn as_slice(&self) -> &[u8] {
        unsafe { slice::from_raw_parts(self.contents_ptr(), self.len()) }
    }

    pub fn as_str(&self) -> &str {
        unsafe { core::str::from_utf8_unchecked(self.as_slice()) }
    }

    pub fn len(&self) -> usize {
        unsafe { self.length }
    }

    // pub fn into_boxed_slice(self) -> Option<Box<[u8]>> {
    //     match unsafe { self.header.as_ref() } {
    //         None => None,
    //         Some(header) => {
    //             let mut out = Vec::new();
    //             // unsafe {
    //             //     out.write(transmute_copy(&*self.header)).unwrap();
    //             //     out.write(slice::from_raw_parts(header.contents_ptr(), self.len()))
    //             //         .unwrap();
    //             // }
    //             Some(out.into_boxed_slice())
    //         }
    //     }
    // }
}

impl Chars {
    pub unsafe fn alloc(length: usize) -> *mut Chars {
        let (layout, [ref_count_offset, length_offset]) = Chars::layout(length);
        let chars = alloc(layout);
        (chars.add(ref_count_offset) as *mut i64).write(0);
        (chars.add(length_offset) as *mut usize).write(length);
        chars as *mut Chars
    }

    pub unsafe fn dealloc(chars: &mut Chars) {
        dealloc(chars as *mut Chars as *mut u8, Chars::layout(chars.len()).0);
    }

    fn layout(length: usize) -> (Layout, [usize; 2]) {
        let ref_count_layout = Layout::new::<i64>();
        let length_layout = Layout::new::<usize>();
        let contents_layout = Layout::array::<u8>(length).unwrap();

        let layout = Layout::from_size_align(0, 1).unwrap();
        let (layout, ref_count_offset) = layout.extend(ref_count_layout).unwrap();
        let (layout, length_offset) = layout.extend(length_layout).unwrap();
        let (layout, _contents_offset) = layout.extend(contents_layout).unwrap();

        (layout, [ref_count_offset, length_offset])
    }

    pub fn contents_ptr(&self) -> *const u8 {
        unsafe { (self as *const Chars as *const u8).add(size_of::<i64>() + size_of::<usize>()) }
    }

    pub fn contents_ptr_mut(&mut self) -> *mut u8 {
        unsafe { (self as *mut Chars as *mut u8).add(size_of::<i64>() + size_of::<usize>()) }
    }
}

impl Display for Chars {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self.as_str(), f)
    }
}

impl Eq for Chars {}

impl PartialEq<Self> for Chars {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl PartialOrd<Self> for Chars {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.as_slice().partial_cmp(other.as_slice())
    }
}

impl Ord for Chars {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_slice().cmp(other.as_slice())
    }
}

impl Debug for Chars {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.as_str(), f)
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
        let chars = Chars::from_str("test");
        let chars = chars.as_ref().unwrap();

        assert_eq!(chars.len(), "test".len());
        assert_eq!(chars.as_str(), "test");
    }

    #[test]
    fn can_clone_chars() {
        let chars1 = Chars::from_bytes(b"test");
        let chars2 = chars1.clone();
        let chars1 = chars1.as_ref().unwrap();
        let chars2 = chars2.as_ref().unwrap();

        assert_eq!(chars1.len(), chars2.len());
        assert_eq!(chars1.as_slice(), chars2.as_slice());
    }
}
