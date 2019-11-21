use std::borrow::Borrow;
use std::env::args;
use std::error::Error;
use std::fs::read_to_string;
use std::path::Path;
use std::process::exit;

mod error;
mod pst;
mod reader;
mod sender;
mod types;

enum Options {
    Send,
    Gallop,
    Proofread,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<_> = args().skip(1).collect();
    if args.len() < 2 {
        println!("usage: fimpp [send|gallop|proofread] report.fpp");
        return Ok(());
    }
    let option = args[0].borrow();
    let option = match option {
        "send" => Options::Send,
        "gallop" => Options::Gallop,
        "proofread" => Options::Proofread,
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
    let ast = reader::read(&report)?;

    match option {
        Options::Send => sender::send_out(&ast, name)?,
        Options::Gallop => sender::gallop(&ast, name)?,
        Options::Proofread => sender::proofread(&ast)?,
    }

    Ok(())
}
