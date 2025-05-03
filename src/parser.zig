const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;

const Instruction = enum {
    i_nop,
    i_add,
    i_sub,
    i_sbn,
    i_mul,
    i_div,
    i_mod,
    i_exp,
    i_shl,
    i_shr,
    i_and,
    i_orr,
    i_xor,
    i_ldr,
    i_str,
    i_ldh,
    i_b,
    i_setf,
    i_brk,
    i_wait,
};

const Condition = enum(u4) {
    al = 0,
    eq = 1,
    ne = 2,
    ng = 3,
    pz = 4,
    lt = 5,
    le = 6,
    gt = 7,
    ge = 8,
    vs = 9,
    vc = 10,
    es = 11,
    ec = 12,
    res_13 = 13,
    res_14 = 14,
    nv = 15,
};

const Argument = enum {
    register,
    immediate,
    imm_ident,
};

pub const Parser = struct {
    lexer: Lexer = undefined,

    pub fn init(self: *Parser, allocator: std.mem.Allocator, input: []u8) !void {
        self.lexer = Lexer{ .input = input };
        return self.lexer.init(allocator);
    }

    pub fn deinit(self: *Parser) void {
        self.lexer.deinit();
    }

    pub fn parse(self: *Parser) !void {
        var token = self.lexer.lex();
        while (token.type != .eof) {
            std.debug.print("{any}: {s}\n", .{token.type, token.slice});
            if (token.type == .instruction) std.debug.print("{any} \n", .{token.instruction});
            token = self.lexer.lex();
        }
    }
};
