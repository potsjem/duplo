const std = @import("std");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;

pub const Type = union(enum) {
    integer: Integer,

    const Integer = struct {
        signed: bool,
        bits: u32,
    };
};

pub const SymbolTable = struct {
    table: StringHashMap(Entry),
    types: ArrayList(Type),

    const Entry = struct {

    };
};
