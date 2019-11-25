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

enum Command {
    Send,
    Gallop,
    Proofread,
}

const TARGET_LINUX: &str = "x86_64-unknown-unknown-elf";
const TARGET_MACOS: &str = "x86_64-unknown-unknown-macho";
const TARGET_WINDOWS: &str = "x86_64-pc-windows-msvc";

#[cfg(target_os = "linux")]
const TARGET_HOST: &str = TARGET_LINUX;

#[cfg(target_os = "macos")]
const TARGET_HOST: &str = TARGET_MACOS;

#[cfg(target_os = "windows")]
const TARGET_HOST: &str = TARGET_WINDOWS;

const HELP_TEXT: &str = "FiM++ 1.0

USAGE:
    fimpp [SUBCOMMAND] <REPORT> [OPTIONS]

ARGS:
   REPORT the report to send

SUBCOMMANDS:
    help\tPrints this message or the help of the given subcommand(s)
    send\tSends (compiles) the report
    gallop\tExecutes the report
    proofread\tPrints out ir of the report";

const HELP_TEXT_SEND: &str = "FiM++ 1.0

Sends (compiles) the report

USAGE:
    fimpp send <REPORT> [to TARGET]

ARGS:
   REPORT the report to send
   TARGET the compile target, one of: [host|linux|macos]";

const HELP_TEXT_GALLOP: &str = "FiM++ 1.0

Executes the report

USAGE:
    fimpp gallop <REPORT>

ARGS:
   REPORT the report to execute";

const HELP_TEXT_PROOFREAD: &str = "FiM++ 1.0

Prints out ir of the report to aid with debugging

USAGE:
    fimpp proofread <REPORT>

ARGS:
   REPORT the report to process";

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<_> = args().skip(1).collect();
    if args.len() < 2 {
        println!("{}", HELP_TEXT);
        return Ok(());
    }
    let cmd = args[0].borrow();
    let cmd = match cmd {
        "send" => Command::Send,
        "gallop" => Command::Gallop,
        "proofread" => Command::Proofread,
        "help" => {
            if args.len() == 1 {
                println!("{}", HELP_TEXT);
            } else if args.len() == 2 {
                match args[1].borrow() {
                    "send" => {
                        println!("{}", HELP_TEXT_SEND);
                    }
                    "gallop" => {
                        println!("{}", HELP_TEXT_GALLOP);
                    }
                    "proofread" => {
                        println!("{}", HELP_TEXT_PROOFREAD);
                    }
                    o => {
                        eprintln!("unrecognized subcommand: {}", o);
                        exit(1);
                    }
                };
            }
            return Ok(());
        }
        o => {
            eprintln!("unrecognized subcommand: {}", o);
            exit(1);
        }
    };

    let path = Path::new(&args[1]);
    let report = read_to_string(path).expect("error opening report");
    let name = path.file_stem().unwrap().to_str().unwrap();
    let ast = reader::read(&report)?;

    match cmd {
        Command::Send => {
            let target = if args.len() == 4 {
                match args[2].borrow() {
                    "to" => match args[3].borrow() {
                        "host" => TARGET_HOST,
                        "linux" => TARGET_LINUX,
                        "macos" => TARGET_MACOS,
                        t => {
                            eprintln!("unrecognized target: {}", t);
                            exit(1);
                        }
                    },
                    o => {
                        eprintln!("unrecognized option: {}", o);
                        exit(1);
                    }
                }
            } else {
                TARGET_HOST
            };
            sender::send_out(&ast, name, target)?
        }
        Command::Gallop => sender::gallop(&ast, name)?,
        Command::Proofread => sender::proofread(&ast)?,
    };

    Ok(())
}

fn report(path: &str) -> String {
    let path = Path::new(&path);
    if path.is_dir() {
        panic!("expected file but got dir");
    }
    read_to_string(path).expect("error opening report")
}
