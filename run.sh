#!/bin/sh

RUST_BACKTRACE=1 cargo run --bin kagc 1> main.S
gcc  -o out main.S std.c

export DYLD_LIBRARY_PATH=./:$DYLD_LIBRARY_PATH
./out