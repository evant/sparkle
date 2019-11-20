#!/usr/bin/bash

cargo run -- send src/hello_canterlot.fpp
cc hello_canterlot.o -o hello_canterlot
./hello_canterlot
