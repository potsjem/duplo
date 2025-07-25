const std = @import("std");
const cwd = std.fs.cwd;
const stdout = std.io.getStdOut().writer();

const DebugAllocator = std.heap.DebugAllocator(.{});

const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const zgen = @import("zg386.zig");

const symbol = @import("symbol.zig");
const SymbolTable = symbol.SymbolTable;

pub fn main() !void {
    var gpa: DebugAllocator = .init;
    const allocator = gpa.allocator();

    const source = try cwd()
        .readFileAllocOptions(allocator, "main.dup", std.math.maxInt(u32), null, @alignOf(u8), 0);
    defer allocator.free(source);

    const tokens = try Lexer.lex(allocator, source);
    defer allocator.free(tokens);

    for (tokens) |token|
        std.debug.print("token.{s}: '{s}'\n", .{@tagName(token.kind), token.slice(source)});

    const ast = try Parser.parse(allocator, tokens);
    defer ast.deinit(allocator);

    for (ast.nodes) |node|
        std.debug.print("node.{s}: '{s}'\n", .{@tagName(node.kind), tokens[node.main].slice(source)});

    var table = SymbolTable.init(allocator);
    try table.put("foo", .{
        .storage = .auto,
        .value = .{ .addr = 1000 },
        .typ = .{ .integer = .{ .signed = false, .bits = 64 } },
    });

    var foo = zgen.Foo{
        .writer = stdout.any(),
    };

    try foo.genlit(10);
    try foo.genaddr(table, "foo");
    //try foo.genlit(20);
    //try foo.genbinop(.eq);
    try foo.genbinop(.add);
}
