#![feature(slice_internals)]
#![no_std]
extern crate alloc;


use alloc::string::{String, ToString};
use core::fmt::{Display, Write};

use core::ptr::{copy_nonoverlapping};

use libc_print::std_name::{eprintln, print, println};

pub use array::Array;

pub use crate::chars::Chars;
use crate::ptr::Ptr;
use crate::rc::Rc;
use crate::stdio::{stdin, stdout};

mod array;
mod as_string_bytes;
mod buff_read;
mod chars;
mod ptr;
mod rc;
mod stdio;

macro_rules! runtime_sym {
    ($name:ident) => {
        (stringify!($name), $name as *const u8)
    };
}

macro_rules! error {
    () => { unsafe { libc::exit(1)}; };
    ($($arg:tt)*) => {
        eprintln!($($arg)*);
        unsafe { libc::exit(1) };
    };
}

pub const SYMBOLS: &'static [(&'static str, *const u8)] = &[
    runtime_sym!(alloc_chars),
    runtime_sym!(println_chars),
    runtime_sym!(print_chars),
    runtime_sym!(println_bool),
    runtime_sym!(print_bool),
    runtime_sym!(println_num),
    runtime_sym!(print_num),
    runtime_sym!(println_array_bool),
    runtime_sym!(print_array_bool),
    runtime_sym!(println_array_num),
    runtime_sym!(print_array_num),
    runtime_sym!(println_array_chars),
    runtime_sym!(print_array_chars),
    runtime_sym!(read_chars),
    runtime_sym!(read_bool),
    runtime_sym!(read_num),
    runtime_sym!(compare_chars),
    runtime_sym!(num_to_chars),
    runtime_sym!(array_get_num),
    runtime_sym!(array_get_bool),
    runtime_sym!(array_get_chars),
    runtime_sym!(array_set_num),
    runtime_sym!(array_set_bool),
    runtime_sym!(array_set_chars),
    runtime_sym!(array_chars_to_chars),
    runtime_sym!(array_num_to_chars),
    runtime_sym!(array_bool_to_chars),
    runtime_sym!(move_chars),
];

#[no_mangle]
pub unsafe extern "C" fn alloc_chars(len: usize) -> *mut Chars {
    Chars::alloc(len)
}

#[no_mangle]
pub extern "C" fn println_chars(str: Ptr<Chars>) {
    println!("{}", str);
}

#[no_mangle]
pub extern "C" fn print_chars(str: Ptr<Chars>) {
    print!("{}", str);
    stdout().flush();
}

#[no_mangle]
pub extern "C" fn println_bool(bool: bool) {
    println!("{}", if bool { "yes" } else { "no" });
}

#[no_mangle]
pub extern "C" fn print_bool(bool: bool) {
    print!("{}", if bool { "yes" } else { "no" });
    stdout().flush();
}

#[no_mangle]
pub extern "C" fn println_num(float: f64) {
    println!("{}", float);
}

#[no_mangle]
pub extern "C" fn print_num(float: f64) {
    print!("{}", float);
    stdout().flush();
}

#[no_mangle]
pub extern "C" fn println_array_bool(array: Ptr<Array<bool>>) {
    println!("{}", array);
}

#[no_mangle]
pub extern "C" fn print_array_bool(array: Ptr<Array<bool>>) {
    print!("{}", array);
}

#[no_mangle]
pub extern "C" fn println_array_num(array: Ptr<Array<f64>>) {
    println!("{}", array);
}

#[no_mangle]
pub extern "C" fn print_array_num(array: Ptr<Array<f64>>) {
    print!("{}", array);
}

#[no_mangle]
pub extern "C" fn println_array_chars(array: Ptr<Array<Ptr<Chars>>>) {
    println!("{}", array);
}

#[no_mangle]
pub extern "C" fn print_array_chars(array: Ptr<Array<Ptr<Chars>>>) {
    print!("{}", array);
}

#[no_mangle]
pub unsafe extern "C" fn read_chars() -> Rc<Chars> {
    Chars::from_bytes(read_line().trim_end().as_bytes())
}

#[no_mangle]
pub unsafe extern "C" fn read_num() -> f64 {
    read_line().trim_end().parse().unwrap_or(0f64)
}

#[no_mangle]
pub unsafe extern "C" fn read_bool() -> bool {
    match read_line().trim_end() {
        "yes" | "true" | "right" | "correct" => true,
        "no" | "false" | "wrong" | "incorrect" => false,
        _ => {
            error!();
        }
    }
}

#[no_mangle]
unsafe fn read_line() -> String {
    let mut out = String::new();
    match stdin().read_line(&mut out) {
        Ok(_) => out,
        Err(_) => {
            error!();
        }
    }
}

#[no_mangle]
pub extern "C" fn compare_chars(a: Ptr<Chars>, b: Ptr<Chars>) -> i32 {
    match unsafe { (a.as_ref(), b.as_ref()) } {
        (Some(a_str), Some(b_str)) => a_str.cmp(b_str) as i32,
        (_, _) => {
            error!("You cannot compare what you do not have");
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn array_get_num(array: Ptr<Array<f64>>, index: usize) -> f64 {
    *array_get(array.as_ref(), index)
}

#[no_mangle]
pub unsafe extern "C" fn array_get_bool(array: Ptr<Array<bool>>, index: usize) -> bool {
    *array_get(array.as_ref(), index)
}

#[no_mangle]
pub unsafe extern "C" fn array_get_chars(
    array: Ptr<Array<Rc<Chars>>>,
    index: usize,
) -> Rc<Chars> {
    array_get(array.as_ref(), index).clone()
}

fn array_get<T>(array: Option<&Array<T>>, index: usize) -> &T {
    match array {
        Some(array) => match array.get(index) {
            Some(value) => value,
            None => {
                error!(
                    "You cannot get the {}'th if you only have {}",
                    index,
                    array.len(),
                );
            }
        },
        None => {
            error!("You cannot get what you do not have");
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn array_set_num(mut array: Ptr<Array<f64>>, index: usize, item: f64) {
    array_set(array.as_mut(), index, item);
}

#[no_mangle]
pub unsafe extern "C" fn array_set_bool(mut array: Ptr<Array<bool>>, index: usize, item: bool) {
    array_set(array.as_mut(), index, item);
}

#[no_mangle]
pub unsafe extern "C" fn array_set_chars(
    mut array: Ptr<Array<Rc<Chars>>>,
    index: usize,
    item: Rc<Chars>,
) {
    array_set(array.as_mut(), index, item.clone());
}

fn array_set<T: Default>(array: Option<&mut Array<T>>, index: usize, item: T) {
    match array {
        Some(array) => {
            if let Err(_) = array.set(index, item) {
                error!("You cannot change what stays the same");
            }
        }
        None => {
            error!("You cannot set what you do not have");
        }
    }
}

#[no_mangle]
pub extern "C" fn num_to_chars(num: f64) -> Rc<Chars> {
    Chars::from_bytes(num.to_string().as_bytes())
}

#[no_mangle]
pub extern "C" fn array_chars_to_chars(array: Ptr<Array<Ptr<Chars>>>) -> Rc<Chars> {
    array_to_chars(array)
}

#[no_mangle]
pub extern "C" fn array_num_to_chars(array: Ptr<Array<f64>>) -> Rc<Chars> {
    array_to_chars(array)
}

#[no_mangle]
pub extern "C" fn array_bool_to_chars(array: Ptr<Array<bool>>) -> Rc<Chars> {
    array_to_chars(array)
}

fn array_to_chars<T: Display>(array: Ptr<Array<T>>) -> Rc<Chars> {
    Chars::from_bytes(array.to_string().as_bytes())
}

#[no_mangle]
pub unsafe extern "C" fn move_chars(src: &Chars, dest: *mut u8) {
    let slice = src.as_slice();
    copy_nonoverlapping(slice.as_ptr(), dest, slice.len());
}

#[cfg(test)]
mod tests {
    use alloc::borrow::ToOwned;
    use alloc::boxed::Box;
    use alloc::vec;
    use alloc::vec::Vec;
    use core::ptr::copy_nonoverlapping;

    use super::*;

    #[test]
    fn test_num_to_chars() {
        let chars = num_to_chars(3.14);
        let chars = chars.as_ref().unwrap();

        assert_eq!(chars.as_str(), "3.14");
    }

    #[test]
    fn test_array_to_chars() {
        let array = Array::const_from([
            Chars::from_str("chocolate"),
            Chars::from_str("apple cinnamon"),
            Chars::from_str("fruit"),
        ]);
        let chars = array_to_chars(Ptr(&*array));
        let chars = chars.as_ref().unwrap();

        assert_eq!(chars.as_str(), "chocolate and apple cinnamon and fruit")
    }

    #[test]
    fn test_array_get() {
        let array = Array::const_from([
            Chars::from_str("chocolate"),
            Chars::from_str("apple cinnamon"),
            Chars::from_str("fruit"),
        ]);

        assert_eq!(array_get(Some(&array), 1).as_ref().unwrap().as_str(), "chocolate")
    }

    #[test]
    fn test_array_set() {
        let mut array: Box<Array<Option<Rc<Chars>>>> = Array::dynamic();

        array_set(Some(&mut array), 1, Some(Chars::from_str("chocolate")));
        array_set(Some(&mut array), 2, Some(Chars::from_str("apple cinnamon")));
        array_set(Some(&mut array), 3, Some(Chars::from_str("fruit")));

        assert_eq!(
            array
                .into_vec()
                .into_iter()
                .map(|e| e.map(|e| e.as_ref().unwrap().as_str().to_owned()))
                .collect::<Vec<_>>(),
            vec![
                Some("chocolate".to_string()),
                Some("apple cinnamon".to_string()),
                Some("fruit".to_string())
            ]
        );
    }

    #[test]
    fn test_alloc_chars() {
        unsafe {
            let bytes = b"test";
            let chars = alloc_chars(bytes.len());
            copy_nonoverlapping(
                bytes.as_ptr(),
                (*chars).contents_ptr_mut(),
                bytes.len(),
            );
            let chars = Rc::from_ptr(chars);
            let chars = chars.as_ref().unwrap();

            assert_eq!(chars.len(), 4);
            assert_eq!(chars.as_str(), "test");
        }
    }

    #[test]
    fn test_move_chars() {
        unsafe {
            let chars = Chars::from_str("test");
            let new_chars = alloc_chars(chars.as_ref().unwrap().len());
            move_chars(chars.as_ref().unwrap(), (*new_chars).contents_ptr_mut());
            let new_chars = Rc::from_ptr(new_chars);
            let new_chars = new_chars.as_ref().unwrap();

            assert_eq!(new_chars.len(), 4);
            assert_eq!(new_chars.as_str(), "test");
        }
    }
}
