const std = @import("std");
const panic = std.debug.panic;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;

const Lexer = @import("lexer.zig");
const Token = Lexer.Token;

const Parser = @import("parser.zig");
const Ast = Parser.Ast;

pub const Type = union(enum) {
    Type,
    Noreturn,
    integer: Integer,
    pointer: Pointer,
    function: Function,

    const Integer = struct {
        signed: bool,
        bits: u32,
    };

    const Pointer = struct {
        child: u32,
    };

    //TODO, add more?
    const Function = struct {
        convention: Convention,

        const Convention = enum {
            auto,
            cdecl,
        };
    };

    pub fn bits(self: Type) u32 {
        return switch (self) {
            .Type,
            .Noreturn => @panic("TODO"),
            .integer => |i| i.bits,
            .pointer,
            .function => 32,
        };
    }
};

pub const SymbolTables = struct {
    tables: ArrayList(SymbolTable),

    pub fn init(allocator: Allocator) SymbolTables {
        return .{
            .tables = .init(allocator),
        };
    }

    //TODO, deep construct
    pub fn scan(
        self: *SymbolTables,
        tree: Ast,
        tokens: []const Token,
        input: [:0]const u8,
        tdx: u32,
        ndx: u32
    ) !?SymbolTable.Entry {
        const node = tree.nodes[ndx];

        switch (node.kind) {
            .root => {
                const table = node.extra.rhs;

                try self.put(0, "i8", .{
                    .storage = undefined,
                    .value = .{ .typ = .{ .integer = .{ .signed = true, .bits = 8 } } },
                    .typ = .Type,
                });

                try self.put(0, "i32", .{
                    .storage = undefined,
                    .value = .{ .typ = .{ .integer = .{ .signed = true, .bits = 32 } } },
                    .typ = .Type,
                });

                const roots = tree.extraSlice(node.extra.lhs);
                for (roots) |rdx| {
                    _ = try self.scan(tree, tokens, input, table, rdx);
                }

                return null;
            },
            .fn_decl => {
                const table = tree.extra[node.extra.rhs];

                //TODO, handle types that arent i32
                const prms = tree.extraSlice(node.extra.rhs+1);
                for (prms, 1..) |prm, i| {
                    const name = tokens[tree.nodes[prm].main-2].slice(input);
                    const p = try self.scan(tree, tokens, input, table, prm)
                        orelse @panic("Non-Type expression found in prm slot");

                    try self.put(table, name, .{
                        .storage = .auto,
                        .value = .{ .addr = 4 + 4 * @as(i32, @intCast(i)) },
                        .typ = p.value.?.typ,
                    });
                }

                const rtyp = try self.scan(tree, tokens, input, table, node.extra.lhs);
                _ = rtyp;

                const name = tokens[node.main+1].slice(input);
                try self.put(tdx, name, .{
                    .storage = .public,
                    .value = null,
                    .typ = .{ .function = .{
                        .convention = .auto,
                    }},
                });

                return null;
            },
            .integer => {
                //TODO, return default largest int
                return null;
            },
            .identifier => {
                const name = tokens[node.main].slice(input);
                std.debug.print("iname: {s}\n", .{name});
                return self.get(tdx, name).?;
            },
            .block => {
                const table = node.extra.rhs;
                _ = table;

                //TODO
                return null;
            },
            .add => {
                const lhs = try self.scan(tree, tokens, input, tdx, node.extra.lhs);
                const rhs = try self.scan(tree, tokens, input, tdx, node.extra.rhs);
                _ = lhs;
                _ = rhs;

                //TODO, check if both are compatible integers

                return null;
            },
            .ret => {
                const typ = try self.scan(tree, tokens, input, tdx, node.extra.lhs);
                _ = typ;

                //TODO, check if typ mathces function return

                return .{
                    .storage = undefined,
                    .value = undefined,
                    .typ = .Noreturn,
                };
            },
            else => |k| panic("TODO, handle kind: {}", .{k}),
        }
    }

    pub fn newTable(self: *SymbolTables, parent: ?u32) !u32 {
        const tdx = self.tables.items.len;

        const table = SymbolTable.init(self.tables.allocator, parent);
        try self.tables.append(table);

        return @intCast(tdx);
    }

    pub fn getTable(self: SymbolTables, tdx: u32) ?*SymbolTable {
        if (tdx >= self.tables.items.len)
            return null;
        return &self.tables.items[tdx];
    }

    pub fn put(self: *SymbolTables, tdx: u32, key: []const u8, val: SymbolTable.Entry) !void {
        return self.tables.items[tdx].table.putNoClobber(key, val);
    }

    pub fn get(self: SymbolTables, tdx: u32, key: []const u8) ?SymbolTable.Entry {
        const entry = self.tables.items[tdx];
        return entry.table.get(key) orelse if (entry.parent) |p| self.get(p, key) else null;
    }
};

pub const SymbolTable = struct {
    parent: ?u32,
    table: StringHashMap(Entry),
    types: ArrayList(Type),

    pub const Entry = struct {
        storage: Storage,
        value: ?Value,
        typ: Type,

        const Storage = enum {
            public,
            auto,
        };

        //TODO, expand size
        pub const Value = union {
            addr: i32,
            iint: i32,
            uint: u32,
            typ: Type,
        };
    };

    pub fn init(allocator: Allocator, parent: ?u32) SymbolTable {
        return .{
            .parent = parent,
            .table = .init(allocator),
            .types = .init(allocator),
        };
    }
};
