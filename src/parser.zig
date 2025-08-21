const std = @import("std");
const panic = std.debug.panic;
const reverse = std.mem.reverse;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Lexer = @import("lexer.zig");
const Token = Lexer.Token;

const symbol = @import("symbol.zig");
const SymbolTables = symbol.SymbolTables;
const SymbolTable = symbol.SymbolTable;
const Entry = SymbolTable.Entry;

const zgen = @import("zg386.zig");
const Foo = zgen.Foo;

const Error = error {
    UnexpectedToken,
    UnexpectedFirstToken,
} || Allocator.Error;

pub const Ast = struct {
    nodes: []Node,
    extra: []u32,

    //TODO, change name
    pub const Local = struct {
        name: ?[]const u8,
        entry: SymbolTable.Entry,
    };

    pub fn extraSlice(self: Ast, idx: u32) []u32 {
        const len = self.extra[idx];
        return self.extra[idx+1..idx+len+1];
    }

    pub fn lvalue(
        self: Ast,
        tokens: []const Token,
        input: [:0]const u8,
        foo: *Foo,
        tables: SymbolTables,
        tdx: u32,
        idx: u32,
    ) !Local {
        const node = self.nodes[idx];

        switch (node.kind) {
            .identifier => {
                const ident = tokens[node.main].slice(input);
                const entry = tables.get(tdx, ident) orelse {
                    panic("Unknown identifier: {s}", .{ident});
                };

                return .{
                    .name = ident,
                    .entry = entry,
                };
            },
            .deref => {
                const lv = try self.lvalue(tokens, input, foo, tables, tdx, node.extra.lhs);
                const typ = tables.types.items[lv.entry.typ.pointer.child];

                //TODO, check if correct behaviour
                //      mostly in multi-level derefs
                try foo.genload(lv);
                //try foo.genpush();

                return .{
                    .name = null,
                    .entry = .{
                        .storage = undefined,
                        .value = undefined,
                        .typ = typ,
                    },
                };
            },
            else => @panic("TODO"),
        }
    }

    //TODO, switch idx/tdx order
    pub fn emit(
        self: Ast,
        tokens: []const Token,
        input: [:0]const u8,
        tables: SymbolTables,
        foo: *Foo,
        idx: u32,
        tdx: u32,
        stack_size: u32, //TODO
    ) !void {
        const node = self.nodes[idx];
        try foo.sgen("//kind: {s}{s}", @tagName(node.kind), "");

        switch (node.kind) {
            .root => {
                const table = node.extra.rhs;
                const roots = self.extraSlice(node.extra.lhs);

                for (roots) |rdx| {
                    try self.emit(tokens, input, tables, foo, rdx, table, stack_size);
                }
            },
            .fn_decl => {
                const table = self.extra[node.extra.rhs];
                const name = tokens[node.main+1].slice(input);

                const locals = try self.listLocals(tables, table, node.extra.lhs);

                var size: u32 = 0;
                for (locals) |local| {
                    try foo.ngen1("//local: {s} => {d}", local.name.?, local.entry.typ.bits());
                    size += @divExact(local.entry.typ.bits(), 8);
                }

                try foo.genpublic(name);
                try foo.genname(name);
                try foo.genentry(size);

                try self.emit(tokens, input, tables, foo, node.extra.lhs, table, size);
                foo.acc = false;
            },
            .fn_call => {
                const body = self.nodes[node.extra.lhs];
                const name = tokens[body.main].slice(input); //TODO, change to expr
                const args = self.extraSlice(node.extra.rhs);

                const entry = tables.get(tdx, name).?;
                const proto = entry.typ.function;

                switch (proto.convention) {
                    .auto,
                    .cdecl => {
                        const acc = foo.acc;
                        reverse(u32, args);

                        for (args) |arg| switch (arg) {
                            0 => {
                                @panic("TODO");
                            },
                            else => {
                                try self.emit(tokens, input, tables, foo, arg, tdx, stack_size);
                                try foo.genpush();
                                foo.acc = false;
                            }
                        };

                        try foo.gencall(name);
                        for (args) |_|
                            try foo.genpop2();

                        reverse(u32, args);
                        foo.acc = acc;
                    },
                }

            },
            .integer => {
                //TODO, check type, and parse correct int-variant
                const slice = tokens[node.main].slice(input);
                const value = try std.fmt.parseInt(u32, slice, 10);

                try foo.genlit(value);
            },
            .identifier => {
                const name = tokens[node.main].slice(input);
                const entry = tables.get(tdx, name).?;
                try foo.genload(.{ .name = name, .entry = entry });
            },
            .block => {
                const table = node.extra.rhs;
                const slice = self.extraSlice(node.extra.lhs);

                for (slice) |ndx| {
                    try self.emit(tokens, input, tables, foo, ndx, table, stack_size);
                    foo.acc = false;
                }
            },
            .add => {
                try self.emit(tokens, input, tables, foo, node.extra.lhs, tdx, stack_size);
                try self.emit(tokens, input, tables, foo, node.extra.rhs, tdx, stack_size);
                try foo.genbinop(.add);
            },
            .sub => {
                try self.emit(tokens, input, tables, foo, node.extra.lhs, tdx, stack_size);
                try self.emit(tokens, input, tables, foo, node.extra.rhs, tdx, stack_size);
                try foo.genbinop(.sub);
            },
            .mul => {
                try self.emit(tokens, input, tables, foo, node.extra.lhs, tdx, stack_size);
                try self.emit(tokens, input, tables, foo, node.extra.rhs, tdx, stack_size);
                try foo.genbinop(.mul);
            },
            .div => {
                try self.emit(tokens, input, tables, foo, node.extra.lhs, tdx, stack_size);
                try self.emit(tokens, input, tables, foo, node.extra.rhs, tdx, stack_size);
                try foo.genbinop(.div);
            },
            .ref => {
                const entry = try self.lvalue(tokens, input, foo, tables, tdx, node.extra.lhs);
                try foo.genaddr(entry);
            },
            .deref => {
                const entry = try self.lvalue(tokens, input, foo, tables, tdx, node.extra.lhs);
                try foo.genload(entry);
            },
            .assign => {
                const entry = try self.lvalue(tokens, input, foo, tables, tdx, node.extra.lhs);
                try self.emit(tokens, input, tables, foo, node.extra.rhs, tdx, stack_size);

                try foo.genstore(entry);
            },
            .logand => {
                const lab = foo.label();

                try self.emit(tokens, input, tables, foo, node.extra.lhs, tdx, stack_size);
                try foo.genbrfalse(lab);
                foo.acc = false;

                try self.emit(tokens, input, tables, foo, node.extra.rhs, tdx, stack_size);
                try foo.genlabel(lab);
                try foo.genbool();
                foo.acc = true;
            },
            .logior => {
                const lab = foo.label();

                try self.emit(tokens, input, tables, foo, node.extra.lhs, tdx, stack_size);
                try foo.genbrtrue(lab);
                foo.acc = false;

                try self.emit(tokens, input, tables, foo, node.extra.rhs, tdx, stack_size);
                try foo.genlabel(lab);
                try foo.genbool();
                foo.acc = true;
            },
            .ternary => {
                const lab0 = foo.label();
                const lab1 = foo.label();

                try self.emit(tokens, input, tables, foo, node.extra.lhs, tdx, stack_size);
                try foo.genbrfalse(lab0);
                foo.acc = false;

                try self.emit(tokens, input, tables, foo, node.extra.rhs+0, tdx, stack_size);
                try foo.genjump(lab1);
                try foo.genlabel(lab0);
                foo.acc = false;

                try self.emit(tokens, input, tables, foo, node.extra.rhs+1, tdx, stack_size);
                try foo.genlabel(lab1);
                foo.acc = true;
            },
            .ret => {
                if (node.extra.lhs != 0)
                    try self.emit(tokens, input, tables, foo, node.extra.lhs, tdx, stack_size);
                try foo.genexit(stack_size);
            },
        }

        try foo.sgen("//end: {s}{s}", @tagName(node.kind), "");
    }

    pub fn deinit(self: Ast, allocator: Allocator) void {
        allocator.free(self.nodes);
        allocator.free(self.extra);
    }

    pub fn debug(
        self: Ast,
        tokens: []const Token,
        input: [:0]const u8,
        idx: u32,
        depth: u32
    ) void {
        const node = self.nodes[idx];

        for (0..depth) |_|
            std.debug.print("  ", .{});

        switch (node.kind) {
            .root =>
                std.debug.print("root\n", .{}),
            else =>
                std.debug.print("{s}\n", .{tokens[node.main].slice(input)}),
        }

        switch (node.kind) {
            .root,
            .block => {
                const slice = self.extraSlice(node.extra.lhs);
                for (slice) |ndx| {
                    self.debug(tokens, input, ndx, depth+1);
                }
            },
            .integer,
            .identifier => {},
            .ref,
            .deref => {
                self.debug(tokens, input, node.extra.lhs, depth+1);
            },
            .add,
            .sub,
            .mul,
            .div,
            .assign,
            .logand,
            .logior => {
                self.debug(tokens, input, node.extra.lhs, depth+1);
                self.debug(tokens, input, node.extra.rhs, depth+1);
            },
            .ternary => {
                self.debug(tokens, input, node.extra.lhs, depth+1);
                self.debug(tokens, input, node.extra.rhs+0, depth+1);
                self.debug(tokens, input, node.extra.rhs+1, depth+1);
            },
            .fn_decl => {
                self.debug(tokens, input, node.extra.lhs+1, depth+1);
                self.debug(tokens, input, node.extra.lhs+0, depth+1);
            },
            .fn_call, //TODO, custom impl for call
            .ret => {
                if (node.extra.lhs != 0)
                    self.debug(tokens, input, node.extra.lhs, depth+1);
            },
        }
    }

    fn listLocals(self: Ast, tables: SymbolTables, tdx: u32, idx: u32) ![]Local {
        var locals = ArrayList(Local).init(tables.tables.allocator);
        try self.listLocalsDo(&locals, tables, tdx, idx);
        return locals.toOwnedSlice();
    }

    fn listLocalsDo(
        self: Ast,
        list: *ArrayList(Local),
        tables: SymbolTables,
        tdx: u32,
        idx: u32
    ) !void {
        const node = self.nodes[idx];

        switch (node.kind) {
            .root,
            .fn_decl => unreachable,
            .fn_call => {
                try self.listLocalsDo(list, tables, tdx, node.extra.lhs);

                for (self.extraSlice(node.extra.rhs)) |arg| switch (arg) {
                    0 => {},
                    else => try self.listLocalsDo(list, tables, tdx, arg),
                };
            },
            .integer,
            .identifier => {},
            .block => {
                const table = node.extra.rhs;
                const slice = self.extraSlice(node.extra.lhs);

                //TODO, add to list
                const instance = tables.getTable(table).?;
                var entries = instance.table.iterator();
                while (entries.next()) |entry| {
                    try list.append(.{
                        .name = entry.key_ptr.*,
                        .entry = entry.value_ptr.*,
                    });
                }

                for (slice) |ndx| {
                    try self.listLocalsDo(list, tables, table, ndx);
                }
            },
            .add,
            .sub,
            .mul,
            .div,
            .logand,
            .logior => {
                try self.listLocalsDo(list, tables, tdx, node.extra.lhs);
                try self.listLocalsDo(list, tables, tdx, node.extra.rhs);
            },
            .ref,
            .deref => {
                try self.listLocalsDo(list, tables, tdx, node.extra.lhs);
            },
            .assign => {
                try self.listLocalsDo(list, tables, tdx, node.extra.rhs);
            },
            .ternary => {
                try self.listLocalsDo(list, tables, tdx, node.extra.lhs);
                try self.listLocalsDo(list, tables, tdx, node.extra.rhs);
                try self.listLocalsDo(list, tables, tdx, node.extra.rhs+1);
            },
            .ret => {
                if (node.extra.lhs != 0)
                    try self.listLocalsDo(list, tables, tdx, node.extra.lhs);
            },
        }
    }
};

const Node = struct {
    main: u32,
    kind: Kind,
    extra: Extra,

    const Kind = enum {
        root,
        fn_decl,
        fn_call,
        integer,
        identifier,
        block,
        add,
        sub,
        mul,
        div,
        ref,
        deref,
        assign,
        logand,
        logior,
        ternary,
        ret,
    };

    const Extra = struct {
        lhs: u32,
        rhs: u32
    };
};

const Op = enum {
    add,
    sub,
    mul,
    div,
    ref,
    deref,
    assign,
    logand,
    logior,
    ret,

    const Power = struct {
        lbp: u8,
        rbp: u8,
    };

    fn kind(self: Op) Node.Kind {
        return switch (self) {
            .add => .add,
            .sub => .sub,
            .mul => .mul,
            .div => .div,
            .ref => .ref,
            .deref => .deref,
            .assign => .assign,
            .logand => .logand,
            .logior => .logior,
            .ret => .ret,
        };
    }

    fn prefixPower(self: Op) ?u8 {
        return switch (self) {
            .ref,
            .deref => 10,
            .ret => 1,
            else => null,
        };
    }

    //TODO, reorganize and standardize
    fn infixPower(self: Op) ?Power {
        return switch (self) {
            .assign => .{ .lbp = 2, .rbp = 3 },
            .logand, .logior => .{ .lbp = 4, .rbp = 5 },
            .add, .sub => .{ .lbp = 6, .rbp = 7 },
            .mul, .div => .{ .lbp = 8, .rbp = 9 },
            else => null,
        };
    }
};

fn pushNode(
    nodes: *ArrayList(Node),
    node: Node,
) !u32 {
    const idx = nodes.items.len;
    try nodes.append(node);
    return @intCast(idx);
}

fn pushExtra(
    extra: *ArrayList(u32),
    node: u32,
) !u32 {
    const idx = extra.items.len;
    try extra.append(node);
    return @intCast(idx);
}

fn pushExtraList(
    extra: *ArrayList(u32),
    nodes: *ArrayList(u32),
) !u32 {
    const idx = extra.items.len;
    try extra.append(@intCast(nodes.items.len));
    try extra.appendSlice(nodes.items);
    nodes.deinit();
    return @intCast(idx);
}

//fn pushProto(
//    extra: *ArrayList(u32),
//    rtyp: u32,
//    body: u32,
//) !u32 {
//    const idx = extra.items.len;
//    try extra.append(rtyp);
//    try extra.append(body);
//    return @intCast(idx);
//}

fn peek(tokens: []const Token, idx: *const u32) Token {
    return tokens[idx.*];
}

fn next(tokens: []const Token, idx: *u32) Token {
    const token = tokens[idx.*];
    idx.* += 1;
    return token;
}

fn skip(tokens: []const Token, idx: *u32) void {
    switch (tokens[idx.*].kind) {
        .eof => {},
        else => idx.* += 1,
    }
}

fn expect(tokens: []const Token, idx: *u32, kind: Token.Kind) !void {
    if (peek(tokens, idx).kind != kind)
        return error.UnexpectedToken;
    skip(tokens, idx);
}

pub fn parse(allocator: Allocator, tokens: []const Token, input: [:0]const u8, tables: *SymbolTables) !Ast {
    var nodes = ArrayList(Node).init(allocator);
    var extra = ArrayList(u32).init(allocator);
    //var protos = ArrayList(FnProto).init(allocator);
    var roots = ArrayList(u32).init(allocator);

    //TODO, replace root with struct
    try nodes.append(undefined);
    const root_table = try tables.newTable(null);

    var idx: u32 = 0;

    while (true) {
        switch (peek(tokens, &idx).kind) {
            .eof => break,
            .@"fn" => {
                const table = try tables.newTable(root_table);

                const odx = idx;
                try expect(tokens, &idx, .@"fn");
                try expect(tokens, &idx, .identifier);
                try expect(tokens, &idx, .@"(");

                var prms = ArrayList(u32).init(extra.allocator);

                while (true) switch (peek(tokens, &idx).kind) {
                    .@")" => {
                        skip(tokens, &idx);
                        break;
                    },
                    else => {
                        try expect(tokens, &idx, .identifier);
                        try expect(tokens, &idx, .@":");
                        const typ = try parseExpr(tokens, input, &idx, &nodes, &extra, tables, table, 0);
                        const tnd = try pushNode(&nodes, typ);
                        try prms.append(tnd);

                        switch (next(tokens, &idx).kind) {
                            .@"," => {},
                            .@")" => break,
                            else => return error.UnexpectedToken,
                        }
                    },
                };

                const rtyp = try parseExpr(tokens, input, &idx, &nodes, &extra, tables, table, 0);
                const body = try parseExpr(tokens, input, &idx, &nodes, &extra, tables, table, 0);
                try expect(tokens, &idx, .@";");

                const bdx = try pushNode(&nodes, body);
                const rdx = try pushNode(&nodes, rtyp);
                _ = rdx;

                const tdx = try pushExtra(&extra, table);
                const pdx = try pushExtraList(&extra, &prms); //TODO, add new function pushExtraProto
                _ = pdx;

                const ndx = try pushNode(&nodes, .{
                    .main = odx,
                    .kind = .fn_decl,
                    .extra = .{
                        .lhs = bdx,
                        .rhs = tdx,
                    },
                });
                try roots.append(ndx);
            },
            else => return error.UnexpectedToken,
        }
    }

    //TODO, replace root with struct
    const edx = try pushExtraList(&extra, &roots);
    nodes.items[0].kind = .root;
    nodes.items[0].extra.lhs = edx;
    nodes.items[0].extra.rhs = root_table;


    return .{
        .nodes = try nodes.toOwnedSlice(),
        .extra = try extra.toOwnedSlice(),
    };
}

fn parseExpr(
    tokens: []const Token,
    input: [:0]const u8,
    idx: *u32,
    nodes: *ArrayList(Node),
    extra: *ArrayList(u32),
    tables: *SymbolTables,
    tdx: u32,
    bp: u8,
) Error!Node {
    var lhs: Node = switch (next(tokens, idx).kind) {
        .integer => .{
            .main = idx.* - 1,
            .kind = .integer,
            .extra = undefined
        },
        .identifier => .{
            .main = idx.* - 1,
            .kind = .identifier,
            .extra = undefined
        },
        .@"&" => b: {
            const odx = idx.* - 1;
            const rbp = Op.prefixPower(.ref).?;
            const rhs = try parseExpr(tokens, input, idx, nodes, extra, tables, tdx, rbp);
            const rnd = try pushNode(nodes, rhs);

            break :b .{
                .main = odx,
                .kind = .ref,
                .extra = .{
                    .lhs = rnd,
                    .rhs = undefined,
                },
            };
        },
        .@"*" => b: {
            const odx = idx.* - 1;
            const rbp = Op.prefixPower(.deref).?;
            const rhs = try parseExpr(tokens, input, idx, nodes, extra, tables, tdx, rbp);
            const rnd = try pushNode(nodes, rhs);

            break :b .{
                .main = odx,
                .kind = .deref,
                .extra = .{
                    .lhs = rnd,
                    .rhs = undefined,
                },
            };
        },
        .@"{" => b: {
            const table = try tables.newTable(tdx);

            const odx = idx.* - 1;
            var elems = ArrayList(u32).init(extra.allocator);

            while (true) switch (peek(tokens, idx).kind) {
                .@"}" => {
                    skip(tokens, idx);
                    break;
                },
                .@"let" => {
                    const ldx = idx.*;
                    skip(tokens, idx);

                    const power = Op.assign.infixPower().?;

                    try expect(tokens, idx, .identifier);
                    try expect(tokens, idx, .@":");
                    const typ = try parseExpr(tokens, input, idx, nodes, extra, tables, table, power.rbp);
                    try expect(tokens, idx, .@"=");
                    const expr = try parseExpr(tokens, input, idx, nodes, extra, tables, table, 0);
                    try expect(tokens, idx, .@";");

                    const ind = try pushNode(nodes, .{
                        .main = ldx + 1,
                        .kind = .identifier,
                        .extra = undefined,
                    });
                    const end = try pushNode(nodes, expr);
                    const mnd = try pushNode(nodes, .{
                        .main = ldx,
                        .kind = .assign,
                        .extra = .{
                            .lhs = ind,
                            .rhs = end,
                        },
                    });

                    try elems.append(mnd);

                    const name = tokens[typ.main-2].slice(input);
                    try tables.put(table, name, .{
                        .storage = .auto,
                        .value = .{ .addr = -4 }, //TODO, null on init, set value later
                        .typ = .{ .integer = .{ .bits = 32, .signed = false } },
                    });
                },
                else => {
                    const rhs = try parseExpr(tokens, input, idx, nodes, extra, tables, table, 0);
                    try expect(tokens, idx, .@";");

                    const rnd = try pushNode(nodes, rhs);
                    try elems.append(rnd);
                },
            };

            const edx = try pushExtraList(extra, &elems);

            break :b .{
                .main = odx,
                .kind = .block,
                .extra = .{
                    .lhs = edx,
                    .rhs = table,
                },
            };
        },
        .@"return" => b: {
            const odx = idx.* - 1;
            const rbp = Op.prefixPower(.ret).?;
            const rhs = parseExpr(tokens, input, idx, nodes, extra, tables, tdx, rbp);
            const rnd = if (rhs) |r| try pushNode(nodes, r) else |err| switch (err) {
                //TODO TODO TODO TODO TODO, fucking wrong btw >:(
                error.UnexpectedFirstToken => 0,
                else => return err,
            };

            break :b .{
                .main = odx,
                .kind = .ret,
                .extra = .{
                    .lhs = rnd,
                    .rhs = undefined,
                },
            };
        },
        .@"if" => b: {
            const odx = idx.* - 1;
            const chs = try parseExpr(tokens, input, idx, nodes, extra, tables, tdx, 0);
            const lhs = try parseExpr(tokens, input, idx, nodes, extra, tables, tdx, 0);
            try expect(tokens, idx, .@"else");
            const rhs = try parseExpr(tokens, input, idx, nodes, extra, tables, tdx, 0);

            const cnd = try pushNode(nodes, chs);
            const lnd = try pushNode(nodes, lhs);
            const rnd = try pushNode(nodes, rhs);
            _ = rnd;

            break :b .{
                .main = odx,
                .kind = .ternary,
                .extra = .{
                    .lhs = cnd,
                    .rhs = lnd,
                },
            };
        },
        else => {
            idx.* -= 1;
            //TODO TODO TODO TODO TODO TODO TODO TODO, wrong btw, still leaks
            return error.UnexpectedFirstToken;
        },
    };

    while (true) {
        const odx = idx.*;
        const op: Op = switch (peek(tokens, idx).kind) {
            .@"+" => .add,
            .@"-" => .sub,
            .@"*" => .mul,
            .@"/" => .div,
            .@"=" => .assign,
            .@"and" => .logand,
            .@"or" => .logior,
            .@"(" => {
                //TODO, add args
                try expect(tokens, idx, .@"(");

                var args = ArrayList(u32).init(extra.allocator);

                while (true) switch (peek(tokens, idx).kind) {
                    .@"," => {
                        skip(tokens, idx);
                        try args.append(0);
                    },
                    .@")" => {
                        skip(tokens, idx);
                        break;
                    },
                    else => {
                        const arg = try parseExpr(tokens, input, idx, nodes, extra, tables, tdx, 0);
                        const gnd = try pushNode(nodes, arg);
                        try args.append(gnd);

                        switch (next(tokens, idx).kind) {
                            .@"," => {},
                            .@")" => break,
                            else => return error.UnexpectedToken,
                        }
                    },
                };

                const lnd = try pushNode(nodes, lhs);
                const gnd = try pushExtraList(extra, &args);

                lhs = .{
                    .main = odx,
                    .kind = .fn_call,
                    .extra = .{
                        .lhs = lnd,
                        .rhs = gnd,
                    },
                };

                continue;
            },
            else => break,
        };

        //TODO, do postfix

        //NOTE, do infix
        if (op.infixPower()) |p| {
            if (p.lbp < bp)
                break;

            skip(tokens, idx);

            const lnd = try pushNode(nodes, lhs);
            const rhs = try parseExpr(tokens, input, idx, nodes, extra, tables, tdx, p.rbp);
            const rnd = try pushNode(nodes, rhs);

            lhs = .{
                .main = odx,
                .kind = op.kind(),
                .extra = .{
                    .lhs = lnd,
                    .rhs = rnd,
                },
            };

            continue;
        }

        panic("Unhandled op: {}", .{op});
    }

    return lhs;
}
