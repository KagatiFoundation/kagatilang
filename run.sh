#!/bin/sh

if [ "$1" = "build" ]; then
    RUST_BACKTRACE=1 KAGC_PATH=/Users/rigelstar/kagc/lib cargo run --bin kagc

elif [ "$1" = "run" ]; then
    RUST_BACKTRACE=1 KAGC_PATH=/Users/rigelstar/kagc/lib cargo run --bin kagc > main.S
    gcc -o out main.S -L. -lbich
    export DYLD_LIBRARY_PATH=./:$DYLD_LIBRARY_PATH
    ./out
else
    echo "Usage: $0 {build|compile}"
    exit 1
fi