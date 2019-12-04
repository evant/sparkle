@echo off
REM Work around https://github.com/rust-lang/rust/issues/54216
SET CARGO_TARGET_DIR=C:\tmp\targets\sparkle
cargo run -- send examples/hello_canterlot.fpp
call "C:\Program Files (x86)\Microsoft Visual Studio\2019\BuildTools\VC\Auxiliary\Build\vcvarsall.bat" x64
link hello_canterlot.obj ucrt.lib /entry:main
