use assert_cmd::cargo::CargoError;
use assert_cmd::Command;

type TestResult = Result<(), CargoError>;

#[cfg(not(target_os = "windows"))]
const EOL: &str = "\n";

#[cfg(target_os = "windows")]
const EOL: &str = "\r\n";

#[test]
fn hello_equestria() -> TestResult {
    let mut cmd = Command::cargo_bin("sparkle")?;

    cmd.arg("gallop").arg("examples/hello_equestria.fpp");

    let out = cmd.output().unwrap().stdout;

    cmd.assert()
        .success()
        .stdout("Hello, Equestria!\n".replace("\n", EOL));

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
         Hello, Applejack\n"
            .replace("\n", EOL),
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
         10\n"
            .replace("\n", EOL),
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
         yes\n"
            .replace("\n", EOL),
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
         yes\n\
         Applejack\n\
         Fluttershy\n\
         Applejack\n\
         nothing\n"
            .replace("\n", EOL),
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
         I'll help them\n\
         I'll release them\n\
         I'll release them\n\
         I'll release them\n"
            .replace("\n", EOL),
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
         yes\n"
            .replace("\n", EOL),
    );

    Ok(())
}

#[test]
fn number_gusser() -> TestResult {
    let mut cmd = Command::cargo_bin("sparkle")?;

    cmd.arg("gallop").arg("examples/number_guesser.fpp");

    cmd.write_stdin("\nhigher\nyes\n")
        .assert()
        .success()
        .stdout(
            "Pick a number between 0 and 100 inclusive\n\
             Is your number 50? If not, please tell me if it's higher or lower: Is your number 75? I got it!\n"
                .replace("\n", EOL),
        );

    Ok(())
}

#[test]
fn cakes() -> TestResult {
    let mut cmd = Command::cargo_bin("sparkle")?;

    cmd.arg("gallop").arg("examples/cakes.fpp");

    cmd.assert().success().stdout(
        "I can bake chocolate and apple cinnamon and fruit cakes!\n\
         My favorite numbers are 1 and 2 and 3.14 and 4 and ok all of them are my favorites!\n\
         but 3.14 is pretty tasty!\n\
         Now I can bake chocolate cake!\n\
         Now I can bake apple cinnamon cake!\n\
         Now I can bake pumpkin cake!\n\
         Now I can bake carrot cake!\n\
         Now I can bake sponge cake!\n",
    );

    Ok(())
}

#[test]
fn sends_hello_equestria() -> TestResult {
    let mut cmd = Command::cargo_bin("sparkle")?;

    cmd.arg("send").arg("examples/hello_equestria.fpp");

    cmd.assert().success();

    let mut report_cmd = if cfg!(target_os = "windows") {
        let mut build = Command::new("canter.bat");

        build.assert().success();

        Command::new("hello_equestria.exe")
    } else {
        let mut build = Command::new("./canter.sh");

        build.assert().success();

        Command::new("./hello_equestria")
    };

    report_cmd
        .assert()
        .success()
        .stdout("Hello, Equestria!".to_string() + EOL);

    Ok(())
}
