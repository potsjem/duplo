const std = @import("std");
const panic = std.debug.panic;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Lexer = @import("lexer.zig");
const Token = Lexer.Token;

const symbol = @import("symbol.zig");
const SymbolTable = symbol.SymbolTable;

const zgen = @import("zg386.zig");
const Foo = zgen.Foo;

pub const Ast = struct {
    nodes: []Node,
    extra: []u32,

    fn extraSlice(self: Ast, idx: u32) []u32 {
        const len = self.extra[idx];
        return self.extra[idx+1..idx+len+1];
    }

    pub fn emit(
        self: Ast,
        tokens: []const Token,
        input: [:0]const u8,
        table: SymbolTable,
        foo: *Foo,
        idx: u32
    ) !void {
        const node = self.nodes[idx];
        try foo.sgen("//kind: {s}{s}", @tagName(node.kind), "");

        switch (node.kind) {
            .root => {
                const slice = self.extraSlice(node.extra.lhs);
                for (slice) |ndx| {
                    try self.emit(tokens, input, table, foo, ndx);
                }
            },
            .fn_decl => {
                const name = tokens[node.main+1].slice(input);

                try foo.genpublic(name);
                try foo.genname(name);
                try foo.genentry();

                try self.emit(tokens, input, table, foo, node.extra.rhs);
                foo.acc = false;
            },
            .fn_call => {
                const body = self.nodes[node.extra.lhs];
                const name = tokens[body.main].slice(input);
                try foo.gencall(name);
            },
            .integer => {
                //TODO, check type, and parse correct int-variant
                const slice = tokens[node.main].slice(input);
                const value = try std.fmt.parseInt(u32, slice, 10);

                try foo.genlit(value);
            },
            .identifier => {
                const name = tokens[node.main].slice(input);
                try foo.genload(table, name);
            },
            .block => {
                const slice = self.extraSlice(node.extra.lhs);
                for (slice) |ndx| {
                    try self.emit(tokens, input, table, foo, ndx);

                    //TODO, figure out if correct
                    foo.acc = false;
                }
            },
            .add => {
                try self.emit(tokens, input, table, foo, node.extra.lhs);
                try self.emit(tokens, input, table, foo, node.extra.rhs);
                try foo.genbinop(.add);
            },
            .sub => {
                try self.emit(tokens, input, table, foo, node.extra.lhs);
                try self.emit(tokens, input, table, foo, node.extra.rhs);
                try foo.genbinop(.sub);
            },
            .mul => {
                try self.emit(tokens, input, table, foo, node.extra.lhs);
                try self.emit(tokens, input, table, foo, node.extra.rhs);
                try foo.genbinop(.mul);
            },
            .div => {
                try self.emit(tokens, input, table, foo, node.extra.lhs);
                try self.emit(tokens, input, table, foo, node.extra.rhs);
                try foo.genbinop(.div);
            },
            .logand => {
                const lab = foo.label();

                try self.emit(tokens, input, table, foo, node.extra.lhs);
                try foo.genbrfalse(lab);
                foo.acc = false;

                try self.emit(tokens, input, table, foo, node.extra.rhs);
                try foo.genlabel(lab);
                try foo.genbool();
                foo.acc = true;
            },
            .logior => {
                const lab = foo.label();

                try self.emit(tokens, input, table, foo, node.extra.lhs);
                try foo.genbrtrue(lab);
                foo.acc = false;

                try self.emit(tokens, input, table, foo, node.extra.rhs);
                try foo.genlabel(lab);
                try foo.genbool();
                foo.acc = true;
            },
            .ternary => {
                const lab0 = foo.label();
                const lab1 = foo.label();

                try self.emit(tokens, input, table, foo, node.extra.lhs);
                try foo.genbrfalse(lab0);
                foo.acc = false;

                try self.emit(tokens, input, table, foo, node.extra.rhs+0);
                try foo.genjump(lab1);
                try foo.genlabel(lab0);
                foo.acc = false;

                try self.emit(tokens, input, table, foo, node.extra.rhs+1);
                try foo.genlabel(lab1);
                foo.acc = true;
            },
            .ret => {
                try self.emit(tokens, input, table, foo, node.extra.lhs);
                try foo.genexit();
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
            .fn_decl,
            .add,
            .sub,
            .mul,
            .div,
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
            .fn_call, //TODO, custom impl for call
            .ret => {
                self.debug(tokens, input, node.extra.lhs, depth+1);
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
            .logand => .logand,
            .logior => .logior,
            .ret => .ret,
        };
    }

    fn prefixPower(self: Op) ?u8 {
        return switch (self) {
            .ret => 1,
            else => null,
        };
    }

    //TODO, reorganize and standardize
    fn infixPower(self: Op) ?Power {
        return switch (self) {
            .logand, .logior => .{ .lbp = 1, .rbp = 2 },
            .add, .sub => .{ .lbp = 3, .rbp = 4 },
            .mul, .div => .{ .lbp = 5, .rbp = 6 },
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

fn peek(input: []const Token, idx: *const u32) Token {
    return input[idx.*];
}

fn next(input: []const Token, idx: *u32) Token {
    const token = input[idx.*];
    idx.* += 1;
    return token;
}

fn skip(input: []const Token, idx: *u32) void {
    switch (input[idx.*].kind) {
        .eof => {},
        else => idx.* += 1,
    }
}

fn expect(input: []const Token, idx: *u32, kind: Token.Kind) !void {
    if (peek(input, idx).kind != kind)
        return error.UnexpectedToken;
    skip(input, idx);
}

pub fn parse(allocator: Allocator, input: []const Token) !Ast {
    var nodes = ArrayList(Node).init(allocator);
    var extra = ArrayList(u32).init(allocator);
    //var protos = ArrayList(FnProto).init(allocator);
    var roots = ArrayList(u32).init(allocator);

    //TODO, replace root with struct
    try nodes.append(undefined);

    var idx: u32 = 0;

    while (true) {
        switch (peek(input, &idx).kind) {
            .eof => break,
            //.integer,
            //.identifier,
            //.@"return" => {
            //    const node = try parseExpr(input, &idx, &nodes, &extra, 0);
            //    const ndx = try pushNode(&nodes, node);
            //    try roots.append(ndx);
            //},
            .@"fn" => {
                const odx = idx;
                try expect(input, &idx, .@"fn");
                try expect(input, &idx, .identifier);
                try expect(input, &idx, .@"(");
                try expect(input, &idx, .@")");
                const rtyp = try parseExpr(input, &idx, &nodes, &extra, 0);
                const body = try parseExpr(input, &idx, &nodes, &extra, 0);
                try expect(input, &idx, .@";");

                const tdx = try pushNode(&nodes, rtyp);
                const bdx = try pushNode(&nodes, body);
                //const pdx = try pushProto(&protos, tbx, bdx);

                const ndx = try pushNode(&nodes, .{
                    .main = odx,
                    .kind = .fn_decl,
                    .extra = .{
                        .lhs = tdx, //TODO, add args
                        .rhs = bdx,
                    },
                });
                try roots.append(ndx);
            },
            else => |k| panic("unexpected token.{s}", .{@tagName(k)}),
        }
    }

    //TODO, replace root with struct
    const edx = try pushExtraList(&extra, &roots);
    nodes.items[0].kind = .root;
    nodes.items[0].extra.lhs = edx;

    return .{
        .nodes = try nodes.toOwnedSlice(),
        .extra = try extra.toOwnedSlice(),
    };
}

fn parseExpr(
    input: []const Token,
    idx: *u32,
    nodes: *ArrayList(Node),
    extra: *ArrayList(u32),
    bp: u8,
) !Node {
    var lhs: Node = switch (next(input, idx).kind) {
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
        .@"{" => b: {
            const odx = idx.* - 1;
            var elems = ArrayList(u32).init(extra.allocator);

            while (true) switch (peek(input, idx).kind) {
                .@"}" => {
                    skip(input, idx);
                    break;
                },
                else => {
                    const rhs = try parseExpr(input, idx, nodes, extra, 0);
                    try expect(input, idx, .@";");

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
                    .rhs = undefined,
                },
            };
        },
        .@"return" => b: {
            const odx = idx.* - 1;
            const rbp = Op.prefixPower(.ret).?;
            const rhs = try parseExpr(input, idx, nodes, extra, rbp);
            const rnd = try pushNode(nodes, rhs);

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
            const chs = try parseExpr(input, idx, nodes, extra, 0);
            const lhs = try parseExpr(input, idx, nodes, extra, 0);
            try expect(input, idx, .@"else");
            const rhs = try parseExpr(input, idx, nodes, extra, 0);

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
        else => |t| panic("Unexpected token: {}\n", .{t}),
    };

    while (true) {
        const odx = idx.*;
        const op: Op = switch (peek(input, idx).kind) {
            .@"+" => .add,
            .@"-" => .sub,
            .@"*" => .mul,
            .@"/" => .div,
            .@"and" => .logand,
            .@"or" => .logior,
            .@"(" => {
                //TODO, add args
                try expect(input, idx, .@"(");
                try expect(input, idx, .@")");

                const lnd = try pushNode(nodes, lhs);

                lhs = .{
                    .main = odx,
                    .kind = .fn_call,
                    .extra = .{
                        .lhs = lnd,
                        .rhs = undefined,
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

            skip(input, idx);

            const lnd = try pushNode(nodes, lhs);
            const rhs = try parseExpr(input, idx, nodes, extra, p.rbp);
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
