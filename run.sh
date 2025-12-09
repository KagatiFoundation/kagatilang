#!/bin/sh

if [[ "$OSTYPE" == "darwin"* ]]; then
    STD_LIB_PATH=/tmp/kaglang
    STD_LIB_NAME=kag
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    STD_LIB_PATH=/tmp/kaglang
    STD_LIB_NAME=kag
fi

if [ "$1" = "build" ]; then
    RUST_BACKTRACE=1 KAGC_PATH=/Users/rigelstar/kagc/lib cargo run --bin kagc

elif [ "$1" = "run" ]; then
    RUST_BACKTRACE=1 KAGC_PATH=/Users/rigelstar/kagc/lib cargo run --bin kagc > main.S
    gcc -o out main.S -L$STD_LIB_PATH -l$STD_LIB_NAME
    export DYLD_LIBRARY_PATH=./:$DYLD_LIBRARY_PATH
    ./out
else
    echo "Usage: $0 {build|run}"
    exit 1
fi