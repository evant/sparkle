@echo off
REM Work around https://github.com/rust-lang/rust/issues/54216
SET CARGO_TARGET_DIR=C:\tmp\targets\sparkle
cargo run -- send examples/hello_canterlot.fpp
REM Linking currently not working on windows, manually do it.
call %1 x64
link hello_canterlot.obj ucrt.lib /entry:main
