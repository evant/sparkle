use alloc::alloc::{alloc, dealloc};
use alloc::boxed::Box;
use alloc::vec::Vec;
use core::alloc::Layout;
use core::fmt;
use core::fmt::{Display, Formatter};
use core::marker::PhantomData;
use core::mem::{size_of, transmute, transmute_copy, ManuallyDrop};
use core::{ptr, slice};

use libc_print::std_name::println;

use crate::as_string_bytes::AsStringBytes;

#[repr(transparent)]
pub struct Chars {
    header: *mut CharsHeader,
}

#[repr(C)]
pub struct CharsHeader {
    ref_count: i64,
    length: usize,
    _phantom: PhantomData<[u8]>,
}

impl Chars {
    pub fn from_bytes(bytes: &[u8]) -> Chars {
        unsafe {
            let header = CharsHeader::alloc(bytes.len());
            ptr::copy_nonoverlapping(bytes.as_ptr(), (*header).contents_ptr_mut(), bytes.len());
            (*header).finalize()
        }
    }

    pub fn append(&self, value: impl AsStringBytes) -> Chars {
        if let (Some(current_bytes), Some(new_bytes)) = (self.as_slice(), value.as_bytes()) {
            unsafe {
                let header = CharsHeader::alloc(current_bytes.len() + new_bytes.len());
                let contents_ptr = (*header).contents_ptr_mut();
                ptr::copy_nonoverlapping(current_bytes.as_ptr(), contents_ptr, current_bytes.len());
                let contents_ptr = contents_ptr.add(current_bytes.len());
                ptr::copy_nonoverlapping(new_bytes.as_ptr(), contents_ptr, new_bytes.len());
                (*header).finalize()
            }
        } else {
            Chars {
                header: ptr::null_mut(),
            }
        }
    }

    pub fn as_slice(&self) -> Option<&[u8]> {
        unsafe {
            match { self.header.as_ref() } {
                None => None,
                Some(header) => Some(slice::from_raw_parts(header.contents_ptr(), self.len())),
            }
        }
    }

    pub fn len(&self) -> usize {
        unsafe { (*self.header).length }
    }

    pub fn into_boxed_slice(self) -> Option<Box<[u8]>> {
        match unsafe { self.header.as_ref() } {
            None => None,
            Some(header) => {
                let mut out = Vec::new();
                // unsafe {
                //     out.write(transmute_copy(&*self.header)).unwrap();
                //     out.write(slice::from_raw_parts(header.contents_ptr(), self.len()))
                //         .unwrap();
                // }
                Some(out.into_boxed_slice())
            }
        }
    }
}

impl Default for Chars {
    fn default() -> Self {
        Chars {
            header: ptr::null_mut(),
        }
    }
}

impl CharsHeader {
    pub unsafe fn alloc(length: usize) -> *mut CharsHeader {
        let (layout, [ref_count_offset, length_offset]) = CharsHeader::layout(length);
        let chars = alloc(layout);
        (chars.add(ref_count_offset) as *mut i64).write(0);
        (chars.add(length_offset) as *mut usize).write(length);
        chars as *mut CharsHeader
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
        unsafe {
            (self as *const CharsHeader as *const u8).add(size_of::<i64>() + size_of::<usize>())
        }
    }

    pub fn contents_ptr_mut(&mut self) -> *mut u8 {
        unsafe { (self as *mut CharsHeader as *mut u8).add(size_of::<i64>() + size_of::<usize>()) }
    }

    pub fn finalize(&mut self) -> Chars {
        Chars {
            header: unsafe { self as *mut CharsHeader },
        }
    }
}

impl Drop for Chars {
    fn drop(&mut self) {
        unsafe {
            if let Some(header) = self.header.as_mut() {
                if header.ref_count >= 0 {
                    header.ref_count -= 1;
                    if header.ref_count < 0 {
                        dealloc(
                            self.header as *mut CharsHeader as *mut u8,
                            CharsHeader::layout(self.len()).0,
                        )
                    }
                }
            }
        }
    }
}

impl Clone for Chars {
    fn clone(&self) -> Self {
        unsafe {
            let ref_count = (*self.header).ref_count;
            if ref_count >= 0 {
                (*self.header).ref_count += 1;
            }
        }
        Chars {
            header: self.header,
        }
    }
}

impl Display for Chars {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", unsafe {
            match self.as_slice() {
                None => "nothing",
                Some(slice) => core::str::from_utf8_unchecked(slice),
            }
        })?;
        Ok(())
    }
}

impl AsStringBytes for Chars {
    fn as_bytes(&self) -> Option<&[u8]> {
        self.as_slice()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constructs_a_chars() {
        let chars = Chars::from_bytes(b"test");

        assert_eq!(chars.len(), "test".len());
        assert_eq!(chars.as_slice(), Some("test".as_bytes()));
    }

    #[test]
    fn can_clone_chars() {
        let chars1 = Chars::from_bytes(b"test");
        let chars2 = chars1.clone();

        assert_eq!(chars1.len(), chars2.len());
        assert_eq!(chars1.as_slice(), chars2.as_slice());
    }
}
