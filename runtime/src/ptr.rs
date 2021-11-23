use alloc::boxed::Box;
use core::fmt::{Display, Formatter};

#[repr(transparent)]
pub struct Ptr<T>(pub *const T);

impl<T> Ptr<T> {
    pub unsafe fn into_box(mut self) -> Option<Box<T>> {
        self.as_mut().map(|ptr| Box::from_raw(ptr))
    }

    pub unsafe fn as_ref(&self) -> Option<&T> {
        self.0.as_ref()
    }

    pub unsafe fn as_mut(&mut self) -> Option<&mut T> {
        (self.0 as *mut T).as_mut()
    }
}

impl<T: Display> Display for Ptr<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match unsafe { self.0.as_ref() } {
            None => write!(f, "nothing"),
            Some(this) => write!(f, "{}", this),
        }
    }
}

impl<T> Default for Ptr<T> {
    fn default() -> Self {
        Ptr(core::ptr::null_mut())
    }
}

impl<T> From<Box<T>> for Ptr<T> {
    fn from(from: Box<T>) -> Self {
        Ptr(Box::leak(from))
    }
}
