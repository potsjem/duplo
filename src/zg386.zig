const std = @import("std");
const panic = std.debug.panic;

const Parser = @import("parser.zig");
const Ast = Parser.Ast;

const symbol = @import("symbol.zig");
const SymbolTables = symbol.SymbolTables;
const Type = symbol.Type;

const GPREFIX = ' ';
const LPREFIX = 'L';

const Segment = enum {
    data,
    text,
};

//TODO, cleanup names
const Op = enum {
    add,
    sub,
    mul,
    div,
    mod,
    shl,
    shr,
    @"and",
    xor,
    ior,
    eq,
    ne,
    lt,
    gt,
    lte,
    gte,
};

pub const Foo = struct {
    writer: std.io.AnyWriter,
    segment: Segment = .text,
    acc: bool = false,
    id: u32 = 0,

    var buffer: [128]u8 = undefined;

    pub fn label(self: *Foo) u32 {
        self.id += 1;
        return self.id;
    }

    fn spill(self: *Foo) !void {
        if (self.acc)
            try self.genpush();
    }

    pub fn genraw(self: Foo, s: []const u8) !void {
        try self.writer.print("{s}", .{s});
    }

    pub fn gen(self: Foo, s: []const u8) !void {
        try self.writer.print("\t{s}\n", .{s});
    }

    pub fn genlabel(self: Foo, id: u32) !void {
        try self.writer.print("{c}{d}:", .{LPREFIX, id});
    }

    //TODO, figure out how to fix n-prm
    pub fn ngen1(self: Foo, comptime fmt: []const u8, inst: []const u8, n: anytype) !void {
        try self.writer.print("\t", .{});
        try self.writer.print(fmt, .{inst, n});
        try self.writer.print("\n", .{});
    }

    fn ngen2(self: Foo, comptime fmt: []const u8, inst: []const u8, n: u32, a: u32) !void {
        try self.writer.print("\t", .{});
        try self.writer.print(fmt, .{inst, n, a});
        try self.writer.print("\n", .{});
    }

    fn lgen1(self: Foo, comptime fmt: []const u8, inst: []const u8, n: u32) !void {
        try self.writer.print("\t", .{});
        try self.writer.print(fmt, .{inst, LPREFIX, n});
        try self.writer.print("\n", .{});
    }

    fn lgen2(self: Foo, comptime fmt: []const u8, v1: u32, v2: u32) !void {
        try self.writer.print("\t", .{});
        try self.writer.print(fmt, .{v1, LPREFIX, v2});
        try self.writer.print("\n", .{});
    }

    pub fn sgen(self: Foo, comptime fmt: []const u8, inst: []const u8, str: []const u8) !void {
        try self.writer.print("\t", .{});
        try self.writer.print(fmt, .{inst, str});
        try self.writer.print("\n", .{});
    }


    fn labname(id: u32) ![]const u8 {
        return std.fmt.bufPrint(&buffer, "{c}{d}", .{LPREFIX, id});
    }

    fn gsym(s: []const u8) ![]const u8 {
        return std.fmt.bufPrint(&buffer, "{c}{s}", .{GPREFIX, s});
    }


    pub fn gendata(self: *Foo) !void {
        if (self.segment != .data)
            try self.cgdata();
        self.segment = .data;
    }

    pub fn gentext(self: *Foo) !void {
        if (self.segment != .text)
            try self.cgtext();
        self.segment = .text;
    }


    fn genprelude(self: Foo) void {
        self.segment = .data;
        try self.gentext();
        self.cgprelude();
    }

    fn genpostlude(self: Foo) void {
        self.cgpostlude();
    }


    pub fn genname(self: Foo, name: []const u8) !void {
        try self.genraw(try gsym(name));
        try self.genraw(":\n");
    }

    pub fn genpublic(self: Foo, name: []const u8) !void {
        try self.cgpublic(try gsym(name));
    }

    //NOTE, diff impl than ref
    pub fn genstore(self: *Foo, tables: SymbolTables, lv: Ast.Local) !void {
        try self.gentext();

        if (lv.entry.typ == .structdef)
            @panic("Not supporting structs fully yet");

        const name = lv.name orelse switch (lv.entry.typ.bits(tables).?) {
            32 => {
                try self.cgpopptr();
                try self.cgstoriw();
                return;
            },
            else => |s| panic("Unhandled bitsize: {}", .{s}),
        };

        _ = name;

        switch (lv.entry.storage) {
            .auto => switch (lv.entry.typ.bits(tables).?) {
                32 => {
                    try self.cgstorlw(lv.entry.value.?.addr);
                },
                else => |s| panic("Unhandled bitsize: {}", .{s}),
            },
            else => switch (lv.entry.typ.bits(tables).?) {
                else => |s| panic("Unhandled bitsize: {}", .{s}),
            },
        }
    }

    //TODO, finish
    //      look at ref impl
    pub fn genload(self: *Foo, tables: SymbolTables, lv: Ast.Local) !void {
        try self.gentext();
        try self.spill();

        if (lv.entry.typ == .structdef)
            @panic("Not supporting structs fully yet");

        switch (lv.entry.storage) {
            .public => switch (lv.entry.typ.bits(tables).?) {
                8 => {
                    try self.cgclear();
                    try self.cgldgb(try gsym(lv.name.?));
                },
                32 => {
                    try self.cgldgw(try gsym(lv.name.?));
                },
                else => |s| panic("Unhandled bitsize: {}", .{s}),
            },
            .auto => switch (lv.entry.typ.bits(tables).?) {
                8 => {
                    try self.cgclear();
                    try self.cgldlb(lv.entry.value.?.addr);
                },
                32 => {
                    try self.cgldlw(lv.entry.value.?.addr);
                },
                else => |s| panic("Unhandled bitsize: {}", .{s}),
            },
        }

        self.acc = true;
    }

    pub fn genaddr(self: *Foo, lv: Ast.Local) !void {
        try self.gentext();
        try self.spill();

        switch (lv.entry.storage) {
            .public => {
                try self.cgldga(try gsym(lv.name.?));
            },
            .auto => {
                try self.cgldla(lv.entry.value.?.addr);
            },
        }

        self.acc = true;
    }

    fn genldlab(self: *Foo, id: u32) !void {
        try self.gentext();
        try self.spill();
        self.cgldlab(id);
        self.acc = true;
    }

    pub fn genlit(self: *Foo, v: u32) !void {
        try self.gentext();
        try self.spill();
        try self.cglit(v);
        self.acc = true;
    }

    //NOTE, probably unused
    fn genargc(self: *Foo) !void {
        try self.gentext();
        try self.spill();
        self.cgargc();
        self.acc = true;
    }


    fn genand(self: *Foo) !void {
        try self.gentext();
        try self.cgpop2();
        try self.cgand();
    }

    fn genior(self: *Foo) !void {
        try self.gentext();
        try self.cgpop2();
        try self.cgior();
    }

    fn genxor(self: *Foo) !void {
        try self.gentext();
        try self.cgpop2();
        try self.cgxor();
    }

    fn genshl(self: *Foo, swapped: bool) !void {
        try self.gentext();
        try self.cgpop2();
        if (swapped) try self.cgswap();
        try self.cgshl();
    }

    fn genshr(self: *Foo, swapped: bool) !void {
        try self.gentext();
        try self.cgpop2();
        if (swapped) try self.cgswap();
        try self.cgshr();
    }

    fn genadd(self: *Foo) !void {
        try self.gentext();
        try self.cgpop2();
        try self.cgadd();
    }

    fn gensub(self: *Foo, swapped: bool) !void {
        try self.gentext();
        try self.cgpop2();
        if (swapped) try self.cgswap();
        try self.cgsub();
    }

    fn genmul(self: *Foo) !void {
        try self.gentext();
        try self.cgpop2();
        try self.cgmul();
    }

    fn gendiv(self: *Foo, swapped: bool) !void {
        try self.gentext();
        try self.cgpop2();
        if (swapped) try self.cgswap();
        try self.cgdiv();
    }

    fn genmod(self: *Foo, swapped: bool) !void {
        try self.gentext();
        try self.cgpop2();
        if (swapped) try self.cgswap();
        try self.cgmod();
    }

    pub fn genbinop(self: *Foo, op: Op) !void {
        try switch (op) {
            .add => self.genadd(),
            .sub => self.gensub(true),
            .mul => self.genmul(),
            .div => self.gendiv(true),
            .mod => self.genmod(true),
            .shl => self.genshl(true),
            .shr => self.genshr(true),
            .@"and" => self.genand(),
            .xor => self.genxor(),
            .ior => self.genior(),
            .eq => self.cgeq(),
            .ne => self.cgne(),
            .lt => self.cglt(),
            .gt => self.cggt(),
            .lte => self.cglte(),
            .gte => self.cggte(),
        };
    }

    pub fn genbool(self: *Foo) !void {
        try self.gentext();
        try self.cgbool();
    }

    //TODO, think about typ vs bits
    pub fn genind(self: *Foo, bits: u32) !void {
        try self.gentext();

        switch (bits) {
            8 => try self.cgindb(),
            32 => try self.cgindw(),
            else => |s| panic("Unhandled bitsize: {}", .{s}),
        }
    }

    pub fn genjump(self: *Foo, id: u32) !void {
        try self.gentext();
        try self.cgjump(id);
    }

    pub fn genbrtrue(self: *Foo, id: u32) !void {
        try self.gentext();
        try self.cgbrtrue(id);
    }

    pub fn genbrfalse(self: *Foo, id: u32) !void {
        try self.gentext();
        try self.cgbrfalse(id);
    }

    pub fn gencall(self: *Foo, name: []const u8) !void {
        try self.gentext();
        try self.spill();
        try self.cgcall(try gsym(name));
        self.acc = true;
    }

    pub fn gencalr(self: *Foo) !void {
        try self.gentext();
        try self.cgcalr();
        self.acc = true;
    }

    pub fn genentry(self: *Foo, size: u32) !void {
        try self.gentext();
        try self.cgentry(size);
    }

    pub fn genexit(self: *Foo, size: u32) !void {
        try self.gentext();
        try self.cgexit(size);
    }

    pub fn genpush(self: *Foo) !void {
        try self.gentext();
        try self.cgpush();
    }

    pub fn genpop2(self: *Foo) !void {
        try self.gentext();
        try self.cgpop2();
    }

    pub fn defglob(self: *Foo, lv: Ast.Local) !void {
        try self.gendata();

        switch (lv.entry.typ) {
            .Type,
            .function => return,
            .integer => {},
            else => |t| panic("TODO, Unhandled type: {}", .{t}),
        }

        if (lv.entry.storage == .public)
            try self.genpublic(lv.name.?);

        try self.genname(lv.name.?);

        switch (lv.entry.typ) {
            .integer => |t| switch (t.bits) {
                32 => if (t.signed)
                        try self.cgdefw(@bitCast(lv.entry.value.?.iint))
                    else
                        try self.cgdefw(lv.entry.value.?.uint),
                else => |s| panic("Unhandled bitsize: {}", .{s}),
            },
            .function => {},
            else => unreachable
        }
    }

    fn cgtext(self: *Foo) !void {
        try self.gen(".text");
    }

    fn cgdata(self: *Foo) !void {
        try self.gen(".data");
    }

    fn cgpublic(self: Foo, name: []const u8) !void {
        try self.ngen1(".globl\t{s} // {d}", name, 0);
    }

    fn cglit(self: *Foo, v: u32) !void {
        try self.ngen1("{s}\t${d},%eax", "movl", v);
    }

    fn cgclear(self: *Foo) !void {
        try self.gen("xorl\t%eax,%eax");
    }


    fn cgldgb(self: *Foo, v: []const u8) !void {
        try self.sgen("{s}\t{s},%al", "movb", v);
    }

    fn cgldgw(self: *Foo, v: []const u8) !void {
        try self.sgen("{s}\t{s},%eax", "movl", v);
    }

    fn cgldlb(self: *Foo, v: i32) !void {
        try self.ngen1("{s}\t{d}(%ebp),%al", "movb", v);
    }

    fn cgldlw(self: *Foo, v: i32) !void {
        try self.ngen1("{s}\t{d}(%ebp),%eax", "movl", v);
    }

    fn cgldla(self: *Foo, v: i32) !void {
        try self.ngen1("{s}\t{d}(%ebp),%eax", "leal", v);
    }

    fn cgldga(self: *Foo, v: []const u8) !void {
        try self.sgen("{s}\t${s},%eax", "movl", v);
    }

    fn cgpopptr(self: *Foo) !void {
        try self.gen("popl\t%edx");
    }

    fn cgstoriw(self: *Foo) !void {
        try self.ngen1("{s}\t%eax,(%edx) // {d}", "movl", 0);
    }

    fn cgstorlw(self: *Foo, v: i32) !void {
        try self.ngen1("{s}\t%eax,{d}(%ebp)", "movl", v);
    }

    fn cgpush(self: *Foo) !void {
        try self.gen("pushl\t%eax");
    }

    fn cgpop2(self: *Foo) !void {
        try self.gen("popl\t%ecx");
    }

    fn cgswap(self: *Foo) !void {
        try self.gen("xchgl\t%eax,%ecx");
    }

    fn cgand(self: *Foo) !void {
        try self.gen("andl\t%ecx,%eax");
    }

    fn cgxor(self: *Foo) !void {
        try self.gen("xorl\t%ecx,%eax");
    }

    fn cgior(self: *Foo) !void {
        try self.gen("orl\t%ecx,%eax");
    }

    fn cgadd(self: *Foo) !void {
        try self.gen("addl\t%ecx,%eax");
    }

    fn cgsub(self: *Foo) !void {
        try self.gen("subl\t%ecx,%eax");
    }

    fn cgmul(self: *Foo) !void {
        try self.gen("imull\t%ecx,%eax");
    }

    fn cgdiv(self: *Foo) !void {
        try self.gen("cdq");
        try self.gen("idivl\t%ecx");
    }

    fn cgmod(self: *Foo) !void {
        try self.cgdiv();
        try self.gen("movl\t%edx,%eax");
    }

    fn cgshl(self: *Foo) !void {
        try self.gen("shll\t%cl,%eax");
    }

    fn cgshr(self: *Foo) !void {
        try self.gen("sarl\t%cl,%eax");
    }

    fn cgcmp(self: *Foo, inst: []const u8) !void {
        const lab = self.label();
        try self.gen("xorl\t%edx,%edx");
        try self.cgpop2();
        try self.gen("cmpl\t%eax,%ecx");
        try self.lgen1("{s}\t{c}{d}", inst, lab);
        try self.gen("incl\t%edx");
        try self.genlabel(lab);
        try self.gen("movl\t%edx,%eax");
    }

    fn cgeq(self: *Foo) !void {
        try self.cgcmp("jne");
    }

    fn cgne(self: *Foo) !void {
        try self.cgcmp("je");
    }

    fn cglt(self: *Foo) !void {
        try self.cgcmp("jge");
    }

    fn cggt(self: *Foo) !void {
        try self.cgcmp("jle");
    }

    fn cglte(self: *Foo) !void {
        try self.cgcmp("jg");
    }

    fn cggte(self: *Foo) !void {
        try self.cgcmp("jl");
    }

    fn cgbool(self: *Foo) !void {
        try self.gen("negl\t%eax");
        try self.gen("sbbl\t%eax,%eax");
        try self.gen("negl\t%eax");
    }

    fn cgbr(self: *Foo, instr: []const u8, id: u32) !void {
        try self.gen("orl\t%eax,%eax");
        try self.lgen1("{s}\t{c}{d}", instr, id);
    }

    fn cgbrtrue(self: *Foo, id: u32) !void {
        try self.cgbr("jnz", id);
    }

    fn cgbrfalse(self: *Foo, id: u32) !void {
        try self.cgbr("jz", id);
    }

    fn cgindb(self: *Foo) !void {
        try self.gen("movl\t%eax,%edx");
        try self.cgclear();
        try self.gen("movb\t(%edx),%al");
    }

    fn cgindw(self: *Foo) !void {
        try self.gen("movl\t(%eax),%eax");
    }

    fn cgjump(self: *Foo, id: u32) !void {
        try self.lgen1("{s}\t{c}{d}", "jmp", id);
    }

    fn cgcall(self: *Foo, name: []const u8) !void {
        try self.sgen("{s}\t{s}", "call", name);
    }

    fn cgentry(self: *Foo, size: u32) !void {
        try self.gen("pushl\t%ebp");
        try self.gen("movl\t%esp,%ebp");
        if (size != 0)
            try self.ngen1("{s}\t${d},%esp", "subl", size);
    }

    fn cgexit(self: *Foo, size: u32) !void {
        if (size != 0)
            try self.ngen1("{s}\t${d},%esp", "addl", size);
        try self.gen("popl\t%ebp");
        try self.gen("ret");
    }

    fn cgdefb(self: *Foo, v: u8) !void {
        try self.ngen1("{s}\t{d}", ".byte", v);
    }

    fn cgdefw(self: *Foo, v: u32) !void {
        try self.ngen1("{s}\t{d}", ".long", v);
    }
};
