use std::fmt::Display;
use std::io::{stdin, stdout, Write};
use std::process::exit;

use array::Array;

use crate::as_string_bytes::AsStringBytes;
use crate::chars::Chars;

mod array;
mod as_string_bytes;
mod chars;

macro_rules! runtime_sym {
    ($name:ident) => {
        (stringify!($name), $name as *const u8)
    };
}

pub const SYMBOLS: &'static [(&'static str, *const u8)] = &[
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
    runtime_sym!(write_num),
];

#[no_mangle]
pub extern "C" fn println_chars(str: *const Chars) {
    unsafe {
        match str.as_ref() {
            None => println!("nothing"),
            Some(str) => {
                println!("{}", std::str::from_utf8_unchecked(str.as_slice()));
            }
        }
    }
}

#[no_mangle]
pub extern "C" fn print_chars(str: *const Chars) {
    unsafe {
        match str.as_ref() {
            None => print!("nothing"),
            Some(str) => {
                print!("{}", std::str::from_utf8_unchecked(str.as_slice()));
            }
        }
        stdout().flush();
    }
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
pub extern "C" fn println_array_bool(array: &Array<bool>) {
    println_array(array);
}

#[no_mangle]
pub extern "C" fn print_array_bool(array: &Array<bool>) {
    print_array(array);
}

#[no_mangle]
pub extern "C" fn println_array_num(array: &Array<f64>) {
    println_array(array);
}

#[no_mangle]
pub extern "C" fn print_array_num(array: &Array<f64>) {
    print_array(array);
}

#[no_mangle]
pub extern "C" fn println_array_chars(array: &Array<Chars>) {
    println_array(array);
}

#[no_mangle]
pub extern "C" fn print_array_chars(array: &Array<Chars>) {
    print_array(array);
}

fn println_array<T: Display>(array: &Array<T>) {
    match write_array(stdout(), array, true) {
        Ok(_) => {}
        Err(_) => exit(1),
    }
}

fn print_array<T: Display>(array: &Array<T>) {
    match write_array(stdout(), array, false) {
        Ok(_) => {
            stdout().flush();
        }
        Err(_) => exit(1),
    }
}

fn write_array<T: Display>(
    mut out: impl Write,
    array: &Array<T>,
    newline: bool,
) -> std::io::Result<()> {
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
pub extern "C" fn read_chars() -> *const Chars {
    Box::leak(Chars::from_bytes(read_line().trim_end().as_bytes()))
}

#[no_mangle]
pub extern "C" fn read_num() -> f64 {
    read_line().trim_end().parse().unwrap_or(0f64)
}

#[no_mangle]
pub extern "C" fn read_bool() -> bool {
    match read_line().trim_end() {
        "yes" | "true" | "right" | "correct" => true,
        "no" | "false" | "wrong" | "incorrect" => false,
        _ => exit(1),
    }
}

fn read_line() -> String {
    let mut out = String::new();
    match stdin().read_line(&mut out) {
        Ok(_) => out,
        Err(_) => exit(1),
    }
}

#[no_mangle]
pub extern "C" fn compare_chars(a: *const Chars, b: *const Chars) -> i32 {
    match unsafe { (a.as_ref(), b.as_ref()) } {
        (Some(a), Some(b)) => {
            let a_str = a.as_slice();
            let b_str = b.as_slice();
            a_str.cmp(b_str) as i32
        }
        (_, _) => {
            eprintln!("You cannot compare what you do not have");
            exit(1);
        }
    }
}

#[no_mangle]
pub extern "C" fn concat_chars_chars(a: Chars, b: Chars) -> *const Chars {
    Box::leak(concat(a, b))
}

#[no_mangle]
pub extern "C" fn concat_chars_bool(a: Chars, b: bool) -> *const Chars {
    Box::leak(concat(a, b))
}

#[no_mangle]
pub extern "C" fn concat_chars_num(a: Chars, b: f64) -> *const Chars {
    Box::leak(concat(a, b))
}

fn concat<A: AsStringBytes, B: AsStringBytes>(a: A, b: B) -> Box<Chars> {
    Chars::empty().append(a).append(b)
}

#[no_mangle]
pub unsafe extern "C" fn write_num(buff: *mut u8, len: usize, num: f64) -> usize {
    let mut slice = std::slice::from_raw_parts_mut(buff, len);
    let str = num.to_string();
    let bytes = str.as_bytes();
    let len = bytes.len();
    slice.write_all(bytes).unwrap();
    return len;
}
