const std = @import("std");
const panic = std.debug.panic;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Lexer = @import("lexer.zig");
const Token = Lexer.Token;

pub const Ast = struct {
    nodes: []Node,
    extra: []u32,

    pub fn deinit(self: Ast, allocator: Allocator) void {
        allocator.free(self.nodes);
        allocator.free(self.extra);
    }
};

const Node = struct {
    main: u32,
    kind: Kind,
    extra: Extra,

    const Kind = enum {
        integer,
        identifier,
        add,
        sub,
        mul,
        div,
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
        };
    }

    fn infixPower(self: Op) ?Power {
        return switch (self) {
            .add, .sub => .{ .lbp = 3, .rbp = 4 },
            .mul, .div => .{ .lbp = 5, .rbp = 6 },
            //else => null,
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

fn peek(input: []const Token, idx: *const u32) Token {
    return input[idx.*];
}

fn skip(input: []const Token, idx: *u32) void {
    switch (input[idx.*].kind) {
        .eof => {},
        else => idx.* += 1,
    }
}

pub fn parse(allocator: Allocator, input: []const Token) !Ast {
    var nodes = ArrayList(Node).init(allocator);
    var extra = ArrayList(u32).init(allocator);

    var idx: u32 = 0;

    while (true) {
        switch (peek(input, &idx).kind) {
            .integer,
            .identifier => {
                const node = try parseExpr(input, &idx, &nodes, &extra, 0);
                try nodes.append(node);
            },
            else => break,
        }
    }

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
    var lhs: Node = switch (peek(input, idx).kind) {
        .integer => .{
            .main = idx.*,
            .kind = .integer,
            .extra = undefined
        },
        .identifier => .{
            .main = idx.*,
            .kind = .identifier,
            .extra = undefined
        },
        else => |t| panic("Unexpected token: {}\n", .{t}),
    };

    skip(input, idx);

    while (true) {
        const odx = idx.*;
        const op: Op = switch (peek(input, idx).kind) {
            .@"+" => .add,
            .@"-" => .sub,
            .@"*" => .mul,
            .@"/" => .div,
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
