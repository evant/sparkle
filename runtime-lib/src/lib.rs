#![feature(lang_items, default_alloc_error_handler)]
#![no_std]
extern crate alloc;
#[cfg(not(test))]
extern crate panic_abort;

use core::alloc::Layout;
use core::ffi::c_void;
use core::mem::ManuallyDrop;

pub use sparkle::{
    alloc_chars, array_bool_to_chars, array_chars_to_chars, array_get_bool, array_get_chars,
    array_get_num, array_num_to_chars, array_set_bool, array_set_chars, array_set_num,
    compare_chars, move_chars, num_to_chars, print_array_bool, print_array_chars, print_array_num,
    print_bool, print_chars, print_num, println_array_bool, println_array_chars, println_array_num,
    println_bool, println_chars, println_num, read_bool, read_chars, read_num,
};

// Define allocator that just calls down to libc malloc/free
struct LibCAllocator;

unsafe impl alloc::alloc::GlobalAlloc for LibCAllocator {
    unsafe fn alloc(&self, layout: alloc::alloc::Layout) -> *mut u8 {
        libc::malloc(layout.size()) as *mut u8
    }

    unsafe fn dealloc(&self, ptr: *mut u8, _layout: alloc::alloc::Layout) {
        libc::free(ptr as *mut c_void);
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        libc::calloc(1, layout.size()) as *mut u8
    }

    unsafe fn realloc(&self, ptr: *mut u8, _layout: Layout, new_size: usize) -> *mut u8 {
        libc::realloc(ptr as *mut c_void, new_size) as *mut u8
    }
}

// Assign our wrapper as the global allocator
#[global_allocator]
static ALLOCATOR: LibCAllocator = LibCAllocator;

#[cfg_attr(not(test), lang = "eh_personality")]
extern "C" fn eh_personality() {}

/// Seems to be stripped by the compiler except when running tests?
/// https://github.com/rust-lang/rust/issues/63348
#[cfg(not(test))]
#[no_mangle]
fn rust_oom() -> ! {
    unsafe { libc::exit(1) }
}