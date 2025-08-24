const std = @import("std");
const cwd = std.fs.cwd;
const stdout = std.io.getStdOut().writer();

const DebugAllocator = std.heap.DebugAllocator(.{});

const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const zgen = @import("zg386.zig");

const symbol = @import("symbol.zig");
const SymbolTables = symbol.SymbolTables;

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

    var tables = SymbolTables.init(allocator);
    const ast = try Parser.parse(allocator, tokens, source, &tables);
    defer ast.deinit(allocator);

    ast.debug(tokens, source, 0, 0);

    _ = try tables.scan(ast, tokens, source, undefined, 0);
    try tables.put(0, "mod", .{
        .storage = .public,
        .value = null,
        .typ = .{ .function = .{
            .convention = .auto,
        }},
    });

    var foo = zgen.Foo{
        .writer = stdout.any(),
    };

    //try foo.genpublic("main");
    //try foo.genname("main");

    //try foo.genentry();
    try ast.emit(tokens, source, tables, &foo, 0, undefined, undefined);

    try foo.genraw(".section .note.GNU-stack,\"\"");

    //try foo.genlit(10);
    //try foo.genaddr(table, "foo");
    ////try foo.genlit(20);
    ////try foo.genbinop(.eq);
    //try foo.genbinop(.add);
}
