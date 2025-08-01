const std = @import("std");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;

pub const Type = union(enum) {
    integer: Integer,
    function: Function,

    const Integer = struct {
        signed: bool,
        bits: u32,
    };

    const Function = struct {
        convention: Convention,

        const Convention = enum {
            auto,
            cdecl,
        };
    };

    pub fn bits(self: Type) u32 {
        return switch (self) {
            .integer => |i| i.bits,
            .function => 32,
        };
    }
};

pub const SymbolTable = struct {
    table: StringHashMap(Entry),
    types: ArrayList(Type),

    const Entry = struct {
        storage: Storage,
        value: ?Value,
        typ: Type,

        const Storage = enum {
            public,
            auto,
        };

        //TODO, expand size
        const Value = union {
            addr: u32,
            iint: i32,
            uint: u32,
        };
    };

    pub fn init(allocator: Allocator) SymbolTable {
        return .{
            .table = .init(allocator),
            .types = .init(allocator),
        };
    }

    pub fn put(self: *SymbolTable, key: []const u8, val: Entry) !void {
        return self.table.putNoClobber(key, val);
    }

    pub fn get(self: SymbolTable, key: []const u8) ?Entry {
        return self.table.get(key);
    }
};
