use alloc::string::String;
use core::ffi::c_void;
use core::fmt;

use core2::io::{Cursor, Error, ErrorKind, Result};


use crate::buff_read::BufReadExt;

const LIBC_STD_IN: i32 = 0;
const LIBC_STD_OUT: i32 = 1;
const LIBC_STD_ERR: i32 = 2;

pub struct LibCReader {
    handle: i32,
    buff: [u8; 1024],
    pos: u64,
    end: usize,
}
pub struct LibCWriter(i32);

#[inline]
pub fn stdin() -> &'static mut LibCReader {
    static mut READER: LibCReader = LibCReader {
        handle: LIBC_STD_IN,
        buff: [0; 1024],
        pos: 0,
        end: 0,
    };
    unsafe { &mut READER }
}

#[inline]
pub fn stdout() -> LibCWriter {
    LibCWriter(LIBC_STD_OUT)
}

#[inline]
pub fn stderr() -> LibCWriter {
    LibCWriter(LIBC_STD_ERR)
}

impl LibCWriter {
    pub fn flush(&mut self) {
        //TODO need this?
        // unsafe {
        //     libc::tcflush(self.0, libc::TCOFLUSH);
        // }
    }
}

impl LibCReader {
    // unsafe to call from multiple threads due to the static buff impl.
    pub unsafe fn read_line(&mut self, out: &mut String) -> Result<()> {
        {
            let mut buff = Cursor::new(&self.buff);
            buff.set_position(self.pos);
            if buff.position() != 0 {
                buff.read_line(out)?;
                self.pos = buff.position();
                if buff.position() as usize == self.end {
                    self.end = 0;
                    buff.set_position(0);
                }
                return Ok(());
            }
        }
        loop {
            let count = libc_read(self.handle, &mut self.buff);
            let mut buff = Cursor::new(&self.buff);
            match count {
                0 => break,
                -1 => return Err(Error::new(ErrorKind::Other, "read error")),
                _ => {
                    let line_count = buff.read_line(out)?;
                    if line_count != count as usize {
                        self.pos = line_count as u64;
                    }
                    break;
                }
            }
        }
        Ok(())
    }
}

impl fmt::Write for LibCWriter {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        libc_println(self.0, s)
    }
}

#[cfg(not(windows))]
#[inline]
fn libc_read(handle: i32, buff: &mut [u8]) -> i64 {
    unsafe { libc::read(handle, buff.as_mut_ptr() as *mut c_void, buff.len()) as i64 }
}

#[cfg(windows)]
#[inline]
fn libc_read(handle: i32, buff: &mut [u8]) -> i64 {
    unsafe { libc::read(handle, buff.as_mut_ptr() as *mut c_void, buff.len() as u32) as i64 }
}

#[cfg(not(windows))]
#[inline]
fn libc_println(handle: i32, msg: &str) -> fmt::Result {
    unsafe {
        libc::write(
            handle,
            msg.as_ptr() as *const c_void,
            msg.len() as libc::size_t,
        );
        Ok(())
    }
}

#[cfg(windows)]
#[inline]
fn libc_println(handle: i32, msg: &str) -> fmt::Result {
    unsafe {
        libc::write(handle, msg.as_ptr() as *const c_void, msg.len() as u32);
        Ok(())
    }
}
