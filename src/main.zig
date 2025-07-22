const std = @import("std");
const stdout = std.io.getStdOut().writer();

const zgen = @import("zg386.zig");

pub fn main() !void {
    var foo = zgen.Foo{
        .writer = stdout.any(),
    };

    try foo.genlit(10);
    try foo.genlit(20);
    try foo.genbinop(.eq);
}
