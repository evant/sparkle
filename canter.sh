#!/usr/bin/env bash

cargo run -- send examples/hello_canterlot.fpp
cc hello_canterlot.o -o hello_canterlot
./hello_canterlot
