#![feature(slice_internals)]

#![no_std]
extern crate alloc;

use alloc::string::{String, ToString};
use core::fmt::{Display, Write};
use core::mem::ManuallyDrop;
use core::ptr::copy_nonoverlapping;

use libc_print::std_name::{eprintln, print, println};

pub use array::Array;

pub use crate::chars::{Chars, CharsHeader};
use crate::stdio::{stdin, stdout};

mod array;
mod as_string_bytes;
mod chars;
mod stdio;
mod buff_read;

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
pub unsafe extern "C" fn alloc_chars(len: usize) -> *mut CharsHeader {
    CharsHeader::alloc(len)
}

#[no_mangle]
pub extern "C" fn println_chars(str: ManuallyDrop<Chars>) {
    println!("{}", *str);
}

#[no_mangle]
pub extern "C" fn print_chars(str: ManuallyDrop<Chars>) {
    print!("{}", *str);
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
pub unsafe extern "C" fn println_array_bool(array: *const Array<bool>) {
    println_array(array.as_ref());
}

#[no_mangle]
pub unsafe extern "C" fn print_array_bool(array: *const Array<bool>) {
    print_array(array.as_ref());
}

#[no_mangle]
pub unsafe extern "C" fn println_array_num(array: *const Array<f64>) {
    println_array(array.as_ref());
}

#[no_mangle]
pub unsafe extern "C" fn print_array_num(array: *const Array<f64>) {
    print_array(array.as_ref());
}

#[no_mangle]
pub unsafe extern "C" fn println_array_chars(array: *const Array<Chars>) {
    println_array(array.as_ref());
}

#[no_mangle]
pub unsafe extern "C" fn print_array_chars(array: *const Array<Chars>) {
    print_array(array.as_ref());
}

fn println_array<T: Display>(array: Option<&Array<T>>) {
    match array {
        Some(array) => match write_array(stdout(), array, true) {
            Ok(_) => {}
            Err(_) => {
                error!();
            }
        },
        None => {
            println!("nothing");
        }
    }
}

fn print_array<T: Display>(array: Option<&Array<T>>) {
    match array {
        Some(array) => match write_array(stdout(), array, false) {
            Ok(_) => {}
            Err(_) => {
                error!();
            }
        },
        None => {
            print!("nothing");
            stdout().flush();
        }
    }
}

fn write_array<T: Display>(
    mut out: impl Write,
    array: &Array<T>,
    newline: bool,
) -> core::fmt::Result {
    for (i, item) in array.as_slice().iter().enumerate() {
        if i != 0 {
            write!(out, " and ")?;
        }
        write!(out, "{}", item)?;
    }
    if newline {
        write!(out, "\n")?;
    }
    Ok(())
}

#[no_mangle]
pub unsafe extern "C" fn read_chars() -> Chars {
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
pub extern "C" fn compare_chars(a: ManuallyDrop<Chars>, b: ManuallyDrop<Chars>) -> i32 {
    match (a.as_slice(), b.as_slice()) {
        (Some(a_str), Some(b_str)) => a_str.cmp(b_str) as i32,
        (_, _) => {
            error!("You cannot compare what you do not have");
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn array_get_num(array: *const Array<f64>, index: usize) -> f64 {
    *array_get(array.as_ref(), index)
}

#[no_mangle]
pub unsafe extern "C" fn array_get_bool(array: *const Array<bool>, index: usize) -> bool {
    *array_get(array.as_ref(), index)
}

#[no_mangle]
pub unsafe extern "C" fn array_get_chars(array: *const Array<Chars>, index: usize) -> Chars {
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
pub unsafe extern "C" fn array_set_num(array: *mut Array<f64>, index: usize, item: f64) {
    array_set(array.as_mut(), index, item);
}

#[no_mangle]
pub unsafe extern "C" fn array_set_bool(array: *mut Array<bool>, index: usize, item: bool) {
    array_set(array.as_mut(), index, item);
}

#[no_mangle]
pub unsafe extern "C" fn array_set_chars(
    array: *mut Array<Chars>,
    index: usize,
    item: ManuallyDrop<Chars>,
) {
    array_set(array.as_mut(), index, (&*item).clone());
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
pub extern "C" fn num_to_chars(num: f64) -> Chars {
    Chars::from_bytes(num.to_string().as_bytes())
}

#[no_mangle]
pub unsafe extern "C" fn array_chars_to_chars(array: *const Array<Chars>) -> Chars {
    array_to_chars(array.as_ref())
}

#[no_mangle]
pub unsafe extern "C" fn array_num_to_chars(array: *const Array<f64>) -> Chars {
    array_to_chars(array.as_ref())
}

#[no_mangle]
pub unsafe extern "C" fn array_bool_to_chars(array: *const Array<bool>) -> Chars {
    array_to_chars(array.as_ref())
}

fn array_to_chars<T: Display>(array: Option<&Array<T>>) -> Chars {
    Chars::from_bytes(
        match array {
            None => "nothing".to_string(),
            Some(array) => array.to_string(),
        }
        .as_bytes(),
    )
}

#[no_mangle]
pub unsafe extern "C" fn move_chars(src: Chars, dest: *mut u8) {
    if let Some(slice) = src.as_slice() {
        copy_nonoverlapping(slice.as_ptr(), dest, slice.len());
    }
}

#[cfg(test)]
mod tests {
    use alloc::vec;
    use alloc::vec::Vec;
    use core::ptr::copy_nonoverlapping;

    use super::*;

    #[test]
    fn test_num_to_chars() {
        let chars = num_to_chars(3.14);

        assert_eq!(chars.as_slice(), Some("3.14".as_bytes()));
    }

    #[test]
    fn test_array_to_chars() {
        let array = Array::const_from([
            Chars::from_bytes(b"chocolate"),
            Chars::from_bytes(b"apple cinnamon"),
            Chars::from_bytes(b"fruit"),
        ]);
        let chars = array_to_chars(Some(&array));

        assert_eq!(
            chars.as_slice(),
            Some("chocolate and apple cinnamon and fruit".as_bytes())
        )
    }

    #[test]
    fn test_array_get() {
        let array = Array::const_from([
            Chars::from_bytes(b"chocolate"),
            Chars::from_bytes(b"apple cinnamon"),
            Chars::from_bytes(b"fruit"),
        ]);

        assert_eq!(
            array_get(Some(&array), 1).as_slice(),
            Some("chocolate".as_bytes())
        )
    }

    #[test]
    fn test_array_set() {
        let mut array = Array::dynamic();

        array_set(Some(&mut array), 1, Chars::from_bytes(b"chocolate"));
        array_set(Some(&mut array), 2, Chars::from_bytes(b"apple cinnamon"));
        array_set(Some(&mut array), 3, Chars::from_bytes(b"fruit"));

        assert_eq!(
            array
                .as_slice()
                .iter()
                .map(|e| e.as_slice())
                .collect::<Vec<_>>(),
            vec![
                Some("chocolate".as_bytes()),
                Some("apple cinnamon".as_bytes()),
                Some("fruit".as_bytes())
            ]
        )
    }

    #[test]
    fn test_alloc_chars() {
        unsafe {
            let bytes = b"test";
            let chars = alloc_chars(bytes.len());
            copy_nonoverlapping(bytes.as_ptr(), (*chars).contents_ptr_mut(), bytes.len());
            let chars = (*chars).finalize();

            assert_eq!(chars.len(), 4);
            assert_eq!(chars.as_slice(), Some("test".as_bytes()));
        }
    }

    #[test]
    fn test_move_chars() {
        unsafe {
            let chars = Chars::from_bytes(b"test");
            let new_chars = CharsHeader::alloc(chars.len());
            move_chars(chars, (*new_chars).contents_ptr_mut());
            let new_chars = (*new_chars).finalize();

            assert_eq!(new_chars.len(), 4);
            assert_eq!(new_chars.as_slice(), Some("test".as_bytes()));
        }
    }
}