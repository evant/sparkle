use std::process::Command;

use assert_cmd::cargo::CargoError;
use assert_cmd::prelude::*;

type TestResult = Result<(), CargoError>;

#[test]
fn hello_canterlot() -> TestResult {
    let mut cmd = Command::cargo_bin("fimpp")?;

    cmd.arg("gallop").arg("examples/hello_canterlot.fpp");

    cmd.assert().success().stdout("Hello, Canterlot!\n");

    Ok(())
}

#[test]
fn say_hello_to_everypony() -> TestResult {
    let mut cmd = Command::cargo_bin("fimpp")?;

    cmd.arg("gallop").arg("examples/hello_everypony.fpp");

    cmd.assert().success().stdout(
        "Hello, Rainbow Dash\n\
         Hello, Pinkie Pie\n\
         Hello, Fluttershy\n\
         Hello, Rarity\n\
         Hello, Applejack\n",
    );

    Ok(())
}

#[test]
fn math() -> TestResult {
    let mut cmd = Command::cargo_bin("fimpp")?;

    cmd.arg("gallop").arg("examples/math.fpp");

    cmd.assert().success().stdout(
        "7\n\
         10\n\
         4\n\
         5\n\
         6\n\
         5.5\n",
    );

    Ok(())
}

#[test]
fn logic() -> TestResult {
    let mut cmd = Command::cargo_bin("fimpp")?;

    cmd.arg("gallop").arg("examples/logic.fpp");

    cmd.assert().success().stdout(
        "yes\n\
         no\n\
         yes\n\
         no\n\
         yes\n\
         no\n\
         no\n",
    );

    Ok(())
}

#[test]
fn sends_hello_canterlot() -> TestResult {
    let mut cmd = Command::cargo_bin("fimpp")?;

    cmd.arg("send").arg("examples/hello_canterlot.fpp");

    cmd.assert().success();

    let mut cc_cmd = Command::new("cc");

    cc_cmd
        .arg("hello_canterlot.o")
        .arg("-o")
        .arg("hello_canterlot");

    cc_cmd.assert().success();

    let mut report_cmd = Command::new("./hello_canterlot");
    report_cmd.assert().success().stdout("Hello, Canterlot!\n");

    Ok(())
}
