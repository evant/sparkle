# sparkle

A compiler for FiM++ written in rust.

The language mostly follows the [1.0 reference](https://docs.google.com/document/d/1gU-ZROmZu0Xitw_pfC1ktCDvJH5rM85TxxQf5pg_xmg/edit). You can find notes about differing implementation details in [docs/notes.md](https://github.com/evant/sparkle/blob/master/docs/notes.md).
You can find several sample programs in the [examples](https://github.com/evant/sparkle/tree/master/examples) dir.

## Building

Building sparkle requires the rust toolchain. The easiest way to obtain this is with [rustup](https://rustup.rs/).

```
git clone https://github.com/evant/sparkle.git
cd sparkle
cargo build --release
./target/release/sparkle help
./target/release/sparkle examples/hello_canterlot.fpp
```

## Usage

You may execute a report directly with `sparkle gallop [report]` or compile to an object file with `sparkle send [report]`. You will then need to use your system's linker to link that object file into an executuable.

### Linux/MacOS
```
sparkle send examples/hello_canterlot.fpp
cc hello_canterlot.o -o hello_canterlot
./hello_canterlot
```

### Windows
(Using the Visual Studio 2019 [Build Tools](https://visualstudio.microsoft.com/downloads/#build-tools-for-visual-studio-2019))
```
"C:\Program Files (x86)\Microsoft Visual Studio\2019\BuildTools\VC\Auxiliary\Build\vcvarsall.bat" x64
sparkle.exe send examples/hello_canterlot.fpp
link hello_canterlot.obj ucrt.lib /entry:main
hello_canterlot.exe
```
