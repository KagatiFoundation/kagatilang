#!/bin/sh

RUST_BACKTRACE=1 cargo run --bin kagc > main.S
gcc -o out main.S -L. -lbich

export DYLD_LIBRARY_PATH=./:$DYLD_LIBRARY_PATH
./out