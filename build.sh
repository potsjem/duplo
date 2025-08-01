#!/bin/sh

set -xe

mkdir -p out

zig build-exe src/main.zig $@ -femit-bin=out/duplo

out/duplo > out/lib.S
gcc -m32 lib/main.c out/lib.S -o out/main
