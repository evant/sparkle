use std::alloc::{alloc, dealloc, Layout};
use std::fmt::{Display, Formatter};

use std::ptr::{null};



type Result<T = ()> = std::result::Result<T, ()>;

#[repr(C)]
pub struct Array<T> {
    pub length: usize,
    contents: *mut T,
    capacity: usize,
}

macro_rules! check_const {
    ($expr:expr) => {
        if $expr.is_const() {
            return Err(());
        }
    };
}

impl<T> Array<T> {
    pub fn as_slice(&self) -> &[T] {
        &unsafe { std::slice::from_raw_parts(self.contents, self.length) }
    }

    pub fn push(&mut self, item: T) -> Result {
        check_const!(self);
        if self.capacity <= self.length {
            self.capacity = self.length + 1 * 2;
            let new_slice_layout = Layout::array::<T>(self.capacity).unwrap().pad_to_align();
            unsafe {
                let slice_ptr = alloc(new_slice_layout) as *mut T;
                if !self.contents.is_null() {
                    std::ptr::copy_nonoverlapping(self.contents, slice_ptr, self.length);
                }
                self.drop_slice();
                self.contents = slice_ptr;
            }
        }
        unsafe {
            self.contents.add(self.length).write(item);
        }
        self.length += 1;
        Ok(())
    }

    pub fn is_const(&self) -> bool {
        let ptr = self as *const Array<T> as i64;
        let slice_ptr = self.contents as i64;
        slice_ptr - ptr == 16
    }

    unsafe fn drop_slice_contents(&mut self) {
        let slice = std::slice::from_raw_parts_mut(self.contents, self.length);
        for item in slice {
            std::ptr::drop_in_place(item as *mut T);
        }
    }

    unsafe fn drop_slice(&mut self) {
        self.drop_slice_contents();
        dealloc(
            self.contents as *mut u8,
            Layout::array::<T>(self.capacity).unwrap(),
        );
    }
}

impl<T> Drop for Array<T> {
    fn drop(&mut self) {
        unsafe {
            if self.is_const() {
                self.drop_slice_contents();
            } else {
                self.drop_slice();
            }
        }
    }
}

impl<T: Clone> Array<T> {
    pub fn const_from_slice(slice: &[T]) -> Box<Array<T>> {
        let length_layout = Layout::new::<usize>();
        let contents_ptr_layout = Layout::new::<*mut T>();
        let contents_layout = Layout::array::<T>(slice.len()).unwrap();

        let layout = Layout::from_size_align(0, 1).unwrap();
        let (layout, length_offset) = layout.extend(length_layout).unwrap();
        let (layout, contents_ptr_offset) = layout.extend(contents_ptr_layout).unwrap();
        let (layout, contents_offset) = layout.extend(contents_layout).unwrap();
        let layout = layout.pad_to_align();

        unsafe {
            let array = alloc(layout);
            (array.add(length_offset) as *mut usize).write(slice.len());
            (array.add(contents_ptr_offset) as *mut usize).write(array as usize + contents_offset);
            let contents_ptr = array.add(contents_offset) as *mut T;
            std::ptr::copy_nonoverlapping(slice.as_ptr(), contents_ptr, slice.len());
            Box::from_raw(array as *mut Array<T>)
        }
    }

    pub fn dynamic() -> Box<Array<T>> {
        let array = Array {
            length: 0,
            contents: null::<T>() as *mut T,
            capacity: 0,
        };
        return Box::new(array);
    }
}

impl<T: Display> Display for Array<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (i, item) in self.as_slice().iter().enumerate() {
            if i != 0 {
                write!(f, " and ")?;
            }
            write!(f, "{}", item)?;
        }
        write!(f, "\n")?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constructs_a_const_array() {
        let array = Array::const_from_slice(&["one", "two", "three"]);

        assert!(array.is_const());
        assert_eq!(array.length, 3);
        assert_eq!(array.as_slice(), ["one", "two", "three"]);
    }

    #[test]
    fn constructs_a_dynamic_array() -> Result {
        let mut array: Box<Array<&'static str>> = Array::dynamic();
        assert!(!array.is_const());

        assert_eq!(array.length, 0);
        assert_eq!(array.as_slice().len(), 0);

        array.push("one")?;
        array.push("two")?;
        array.push("three")?;

        assert_eq!(array.length, 3);
        assert_eq!(array.as_slice(), ["one", "two", "three"]);

        Ok(())
    }
}
