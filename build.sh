#!/bin/sh

set -xe

zig build-exe src/main.zig $@
