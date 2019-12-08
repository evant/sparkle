# sparkle

A compiler for FiM++ written in rust.

The language mostly follows the [1.0 reference](https://docs.google.com/document/d/1gU-ZROmZu0Xitw_pfC1ktCDvJH5rM85TxxQf5pg_xmg/edit).
You can find notes about differing implementation details in [docs/notes.md](https://github.com/evant/sparkle/blob/master/docs/notes.md).
You can find several sample programs in the [examples](https://github.com/evant/sparkle/tree/master/examples) dir.

## Building

Building sparkle requires the rust toolchain. The easiest way to obtain this is with [rustup](https://rustup.rs/).

```
git clone https://github.com/evant/sparkle.git
cd sparkle
cargo build --release
./target/release/sparkle help
./target/release/sparkle gallop examples/hello_equestria.fpp
```

## Usage

You may execute a report directly with `sparkle gallop [report]` or compile to an executable with 
`sparkle send [report]`.

#### Linux/MacOS
```
sparkle send examples/hello_equestria.fpp
./hello_equestria
```

#### Windows
You currently need to link the output on windows, this is tempoary until I can figure out how to invoke the linker.

(Using the Visual Studio 2019 [Build Tools](https://visualstudio.microsoft.com/downloads/#build-tools-for-visual-studio-2019))
```
"C:\Program Files (x86)\Microsoft Visual Studio\2019\BuildTools\VC\Auxiliary\Build\vcvarsall.bat" x64
sparkle.exe send examples/hello_equestria.fpp
link hello_equestria.obj ucrt.lib /entry:main
hello_canterlot.exe
```

### Cross Compiling

You may cross-compile with `spakle send [report] to [linux|macos|windows]`. This will generate an object file for the 
given platform. You will still need to invoke that platform's linker to generate the final executable, but you won't 
need sparkle compiled for that platform.

#### Linux/MacOS

```
cc hello_equestria.o -o hello_equestria
```

#### Windows

```
"C:\Program Files (x86)\Microsoft Visual Studio\2019\BuildTools\VC\Auxiliary\Build\vcvarsall.bat" x64
link hello_equestria.obj ucrt.lib /entry:main
```
