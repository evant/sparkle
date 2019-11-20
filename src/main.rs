use std::borrow::Borrow;
use std::env::args;
use std::fs::read_to_string;
use std::path::Path;
use std::process::exit;

mod error;
mod pst;
mod reader;
mod sender;

enum Options {
    Send,
    Gallop,
}

fn main() {
    let args: Vec<_> = args().skip(1).collect();
    if args.len() < 2 {
        println!("usage: fimpp [send|gallop] report.fpp");
        return;
    }
    let option = args[0].borrow();
    let option = match option {
        "send" => Options::Send,
        "gallop" => Options::Gallop,
        o => {
            eprintln!("unrecognized option: {}", o);
            exit(1);
        }
    };
    let path = &args[1];
    let path = Path::new(&path);
    if path.is_dir() {
        panic!("expected file but got dir");
    }
    let report = read_to_string(path).expect("error opening report");
    let name = path.file_stem().unwrap().to_str().unwrap();
    let ast = reader::read(&report).unwrap();

    match option {
        Options::Send => sender::send_out(&ast, name).unwrap(),
        Options::Gallop => sender::gallop(&ast, name).unwrap(),
    }
}
