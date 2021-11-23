use alloc::alloc::{alloc, dealloc};
use alloc::boxed::Box;
use alloc::vec::Vec;
use core::alloc::Layout;
use core::cmp::max;
use core::fmt;
use core::fmt::{Display, Formatter};
use core::mem::forget;
use core::{ptr, slice};



type Result<T = ()> = core::result::Result<T, ()>;

#[repr(C)]
pub struct Array<T> {
    length: usize,
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
    pub fn const_from<const N: usize>(slice: [T; N]) -> Box<Array<T>> {
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
            ptr::write(array.add(length_offset).cast(), N);
            ptr::write(
                array.add(contents_ptr_offset).cast(),
                array as usize + contents_offset,
            );

            let contents_ptr = array.add(contents_offset) as *mut T;
            ptr::copy_nonoverlapping(slice.as_ptr(), contents_ptr, N);

            // Don't drop slice as we own it now.
            forget(slice);

            Box::from_raw(array as *mut Array<T>)
        }
    }

    pub fn dynamic() -> Box<Array<T>> {
        let array = Array {
            length: 0,
            contents: ptr::null::<T>() as *mut T,
            capacity: 0,
        };
        return Box::new(array);
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn as_slice(&self) -> &[T] {
        &unsafe { slice::from_raw_parts(self.contents, self.length) }
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        let index = index - 1; // arrays are 1-indexed
        self.as_slice().get(index)
    }

    pub fn push(&mut self, item: T) -> Result {
        check_const!(self);
        self.ensure_capacity(self.length);
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

    fn ensure_capacity(&mut self, last_index: usize) {
        if self.capacity <= last_index {
            self.capacity = max((self.length + 1) * 2, last_index);
            let new_slice_layout = Layout::array::<T>(self.capacity).unwrap();
            unsafe {
                let slice_ptr = alloc(new_slice_layout) as *mut T;
                if !self.contents.is_null() {
                    ptr::copy_nonoverlapping(self.contents, slice_ptr, self.length);
                }
                self.drop_slice();
                self.contents = slice_ptr;
            }
        }
    }

    unsafe fn drop_slice_contents(&mut self) {
        let slice = slice::from_raw_parts_mut(self.contents, self.length);
        for item in slice {
            ptr::drop_in_place(item as *mut T);
        }
    }

    unsafe fn drop_slice(&mut self) {
        dealloc(
            self.contents as *mut u8,
            Layout::array::<T>(self.capacity).unwrap(),
        );
    }

    pub fn into_vec(self) -> Vec<T> {
        let vec = unsafe { Vec::from_raw_parts(self.contents, self.length, self.capacity) };
        forget(self);
        return vec;
    }
}

impl<T: Default> Array<T> {
    pub fn set(&mut self, index: usize, item: T) -> Result {
        check_const!(self);
        let index = index - 1; // arrays are 1-indexed
        self.ensure_capacity(index);
        if index >= self.length {
            for offset in index..self.length {
                unsafe { self.contents.add(offset).write(T::default()) }
            }
            self.length = index + 1;
        }
        unsafe {
            self.contents.add(index).write(item);
        }
        Ok(())
    }
}

impl<T> Drop for Array<T> {
    fn drop(&mut self) {
        unsafe {
            self.drop_slice_contents();
            if !(self.is_const()) {
                self.drop_slice();
            }
        }
    }
}

impl<T: Display> Display for Array<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (i, item) in self.as_slice().iter().enumerate() {
            if i != 0 {
                write!(f, " and ")?;
            }
            write!(f, "{}", item)?;
        }
        Ok(())
    }
}

impl<T> From<Array<T>> for Vec<T> {
    fn from(array: Array<T>) -> Self {
        array.into_vec()
    }
}

#[cfg(test)]
mod tests {
    use alloc::boxed::Box;

    use super::*;

    #[test]
    fn constructs_a_const_array() {
        let array = Array::const_from(["one", "two", "three"]);

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

    #[test]
    fn dynamic_array_grows_on_set() -> Result {
        let mut array: Box<Array<&'static str>> = Array::dynamic();
        array.set(1, "one")?;
        array.set(2, "two")?;
        array.set(3, "three")?;

        assert_eq!(array.length, 3);
        assert_eq!(array.as_slice(), ["one", "two", "three"]);

        Ok(())
    }
}
