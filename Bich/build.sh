#!/bin/sh

BUILD_PATH=../

gcc -c -Wall -Werror -fpic sock.c io.c
gcc -shared -o $BUILD_PATH/libbich.so sock.o io.o