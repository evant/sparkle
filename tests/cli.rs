use std::io;
use std::process::Command;

use assert_cmd::cargo::CargoError;
use assert_cmd::prelude::*;
use predicates::prelude::*;

#[test]
fn hello_canterlot() -> Result<(), CargoError> {
    let mut cmd = Command::cargo_bin("fimpp")?;

    cmd.arg("gallop")
        .arg("examples/hello_canterlot.fpp");

    cmd.assert()
        .success()
        .stdout("Hello, Canterlot!\n");

    Ok(())
}

#[test]
fn say_hello_to_everypony() -> Result<(), CargoError> {
    let mut cmd = Command::cargo_bin("fimpp")?;

    cmd.arg("gallop")
        .arg("examples/hello_everypony.fpp");

    cmd.assert()
        .success()
        .stdout("Hello, Rainbow Dash\n\
        Hello, Pinkie Pie\n\
        Hello, Fluttershy\n\
        Hello, Rarity\n\
        Hello, Applejack\n");

    Ok(())
}