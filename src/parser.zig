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

    fn kind(self: Op) Node.Kind {
        return switch (self) {
            .add => .add,
            .sub => .sub,
            .mul => .mul,
            .div => .div,
        };
    }
};

fn pushNode(
    nodes: *ArrayList(Node),
    node: Node,
) !void {
    const idx = nodes.items.len;
    try nodes.append(node);
    return idx;
}

fn pushExtra(
    extra: *ArrayList(u32),
    node: u32,
) !void {
    const idx = extra.items.len;
    try extra.append(node);
    return idx;
}

fn peek(input: []const u8, idx: *const u32) Token {
    return input[idx.*];
}

fn skip(input: []const u8, idx: *u32) void {
    switch (input[idx.*]) {
        .eof => {},
        else => idx.* += 1,
    }
}

pub fn parse(allocator: Allocator, input: []const Token) !Ast {
    var nodes = ArrayList(Node).init(allocator);
    var extra = ArrayList(u32).init(allocator);

    _ = input;

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
    var lhs: Node = switch (peek(input, idx)) {
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
        const op: Op = switch (peek(input, idx)) {
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
            const rhs = try parseExpr(input, idx, nodes, extra);
            const rnd = try pushNode(nodes, rhs);

            lhs = .{
                .main = odx,
                .kind = op.kind(),
                .extra = .{
                    .lhs = lnd,
                    .rhs = rnd,
                },
            };
        }

        panic("Unhandled op: {}", .{op});
    }

    return lhs;
}
