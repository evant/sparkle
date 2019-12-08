@echo off
REM Work around https://github.com/rust-lang/rust/issues/54216
SET CARGO_TARGET_DIR=C:\tmp\targets\sparkle
cargo run -- send examples/hello_equestria.fpp
