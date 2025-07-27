#!/bin/sh

set -xe

mkdir -p out

zig build-exe src/main.zig $@

./main > out/lib.S
gcc -m32 main.c out/lib.S
