use core::fmt::Debug;
use core::fmt::Display;
use core::fmt::Formatter;
use core::ptr;

pub trait ReferenceCounted {
    fn inc_ref_count(&mut self);

    fn dec_ref_count(&mut self) -> bool;

    unsafe fn dealloc(&mut self);
}

#[derive(Debug)]
#[repr(transparent)]
pub struct Rc<T: ReferenceCounted>(*mut T);

impl<T: ReferenceCounted> Rc<T> {
    pub unsafe fn from_ptr(ptr: *mut T) -> Rc<T> {
        Rc(ptr)
    }

    pub fn as_ref(&self) -> Option<&T> {
        unsafe { self.0.as_ref() }
    }

    pub fn as_mut(&mut self) -> Option<&mut T> {
        unsafe { self.0.as_mut() }
    }
}

impl<T: ReferenceCounted> Clone for Rc<T> {
    fn clone(&self) -> Self {
        let ptr = self.0;
        match unsafe { ptr.as_mut() } {
            None => Rc(ptr::null_mut()),
            Some(ptr) => {
                ptr.inc_ref_count();
                Rc(ptr)
            }
        }
    }
}

impl<T: ReferenceCounted> Default for Rc<T> {
    fn default() -> Self {
        Rc(ptr::null_mut())
    }
}

impl<T: ReferenceCounted> Drop for Rc<T> {
    fn drop(&mut self) {
        let ptr = self.0;
        unsafe {
            if let Some(ptr) = ptr.as_mut() {
                if ptr.dec_ref_count() {
                    ptr.dealloc()
                }
            }
        }
    }
}

impl<T: ReferenceCounted + Display> Display for Rc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match unsafe { self.0.as_ref() } {
            None => write!(f, "nothing"),
            Some(this) => write!(f, "{}", this),
        }
    }
}
