use std::process::Command;

use assert_cmd::cargo::CargoError;
use assert_cmd::prelude::*;

type TestResult = Result<(), CargoError>;

#[test]
fn hello_canterlot() -> TestResult {
    let mut cmd = Command::cargo_bin("sparkle")?;

    cmd.arg("gallop").arg("examples/hello_canterlot.fpp");

    cmd.assert().success().stdout("Hello, Canterlot!\n");

    Ok(())
}

#[test]
fn say_hello_to_everypony() -> TestResult {
    let mut cmd = Command::cargo_bin("sparkle")?;

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
    let mut cmd = Command::cargo_bin("sparkle")?;

    cmd.arg("gallop").arg("examples/math.fpp");

    cmd.assert().success().stdout(
        "7\n\
         10\n\
         4\n\
         5\n\
         6\n\
         5.5\n\
         6\n\
         11\n\
         10\n",
    );

    Ok(())
}

#[test]
fn logic() -> TestResult {
    let mut cmd = Command::cargo_bin("sparkle")?;

    cmd.arg("gallop").arg("examples/logic.fpp");

    cmd.assert().success().stdout(
        "yes\n\
         no\n\
         yes\n\
         no\n\
         yes\n\
         no\n\
         no\n\
         yes\n",
    );

    Ok(())
}

#[test]
fn variables() -> TestResult {
    let mut cmd = Command::cargo_bin("sparkle")?;

    cmd.arg("gallop").arg("examples/variables.fpp");

    cmd.assert().success().stdout(
        "0\n\
         6\n\
         Tallulah\n\
         yes\n",
    );

    Ok(())
}

#[test]
fn branches() -> TestResult {
    let mut cmd = Command::cargo_bin("sparkle")?;

    cmd.arg("gallop").arg("examples/branches.fpp");

    cmd.assert().success().stdout(
        "I wish I were a tree\n\
         I'll help them\n\
         I'll help them\n\
         I'll help them\n",
    );

    Ok(())
}

#[test]
fn comparisons() -> TestResult {
    let mut cmd = Command::cargo_bin("sparkle")?;

    cmd.arg("gallop").arg("examples/comparisons.fpp");

    cmd.assert().success().stdout(
        "yes\n\
         yes\n\
         yes\n\
         no\n\
         yes\n\
         yes\n",
    );

    Ok(())
}

#[test]
fn sends_hello_canterlot() -> TestResult {
    let mut cmd = Command::cargo_bin("sparkle")?;

    cmd.arg("send").arg("examples/hello_canterlot.fpp");

    cmd.assert().success();

    let mut report_cmd = if cfg!(target_os = "windows") {
        let tool = cc::windows_registry::find_tool("x86_64-pc-windows-msvc", "cl.exe").unwrap();
        //TODO: a better way?
        let env_path = tool.path().parent().unwrap().parent().unwrap().parent().unwrap().parent().unwrap().parent().unwrap().parent().unwrap().parent().unwrap().join("Auxiliary\\Build\\vcvarsall.bat");

        let mut build = Command::new("canter.bat");
        build.arg(env_path);

        build.assert().success();

        Command::new("hello_canterlot.exe")
    } else {
        let mut build = Command::new("./canter.sh");

        build.assert().success();

        Command::new("./hello_canterlot")
    };

    report_cmd.assert().success().stdout("Hello, Canterlot!\n");

    Ok(())
}
