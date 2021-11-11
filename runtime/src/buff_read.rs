use alloc::string::String;
use alloc::vec::Vec;
use core::slice::memchr;

use core2::io::{BufRead, Error, ErrorKind, Result};

pub trait BufReadExt: BufRead {
    fn read_line(&mut self, buf: &mut String) -> Result<usize> {
        append_to_string(buf, |b| read_until(self, b'\n', b))
    }
}

impl<T: BufRead> BufReadExt for T {}

struct Guard<'a> {
    buf: &'a mut Vec<u8>,
    len: usize,
}

fn append_to_string<F>(buf: &mut String, f: F) -> Result<usize>
where
    F: FnOnce(&mut Vec<u8>) -> Result<usize>,
{
    unsafe {
        let mut g = Guard {
            len: buf.len(),
            buf: buf.as_mut_vec(),
        };
        let ret = f(g.buf);
        if core::str::from_utf8(&g.buf[g.len..]).is_err() {
            ret.and_then(|_| {
                Err(Error::new(
                    ErrorKind::InvalidData,
                    &"stream did not contain valid UTF-8",
                ))
            })
        } else {
            g.len = g.buf.len();
            ret
        }
    }
}

fn read_until<R: BufRead + ?Sized>(r: &mut R, delim: u8, buf: &mut Vec<u8>) -> Result<usize> {
    let mut read = 0;
    loop {
        let (done, used) = {
            let available = match r.fill_buf() {
                Ok(n) => n,
                Err(ref e) if e.kind() == ErrorKind::Interrupted => continue,
                Err(e) => return Err(e),
            };
            match memchr::memchr(delim, available) {
                Some(i) => {
                    buf.extend_from_slice(&available[..=i]);
                    (true, i + 1)
                }
                None => {
                    buf.extend_from_slice(available);
                    (false, available.len())
                }
            }
        };
        r.consume(used);
        read += used;
        if done || used == 0 {
            return Ok(read);
        }
    }
}
