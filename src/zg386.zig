const std = @import("std");

const GPREFIX = 'C';
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
    segment: Segment = .data,
    acc: bool = false,
    id: u32 = 0,

    var buffer: [128]u8 = undefined;

    fn label(self: *Foo) u32 {
        self.id += 1;
        return self.id;
    }

    fn spill(self: *Foo) !void {
        if (self.acc)
            try self.genpush();
    }

    fn genraw(self: Foo, s: []const u8) !void {
        try self.writer.print("{s}", .{s});
    }

    fn gen(self: Foo, s: []const u8) !void {
        try self.writer.print("\t{s}\n", .{s});
    }

    fn genlab(self: Foo, id: u32) !void {
        try self.writer.print("{c}{d}:", .{LPREFIX, id});
    }

    fn ngen1(self: Foo, comptime fmt: []const u8, inst: []const u8, n: u32) !void {
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

    fn sgen(self: Foo, comptime fmt: []const u8, inst: []const u8, str: []const u8) !void {
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


    fn gendata(self: *Foo) !void {
        if (self.segment != .data)
            try self.cgdata();
        self.segment = .data;
    }

    fn gentext(self: *Foo) !void {
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


    fn genname(self: Foo, name: []const u8) !void {
        try self.genraw(try gsym(name));
        try self.genraw(":");
    }

    fn genpublic(self: Foo, name: []const u8) !void {
        self.cgpublic(try gsym(name));
    }


    fn genaddr(self: Foo, y: u32) !void {
        _ = self;
        _ = y;
        unreachable;
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

    fn genpush(self: *Foo) !void {
        try self.gentext();
        try self.cgpush();
    }



    fn cglit(self: *Foo, v: u32) !void {
        try self.ngen1("{s}\t${d},%eax", "movl", v);
    }

    fn cgtext(self: *Foo) !void {
        try self.gen(".text");
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
        try self.genlab(lab);
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
};
