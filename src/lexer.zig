//! The lexer splits the input into tokens to be semantically analysed by the parser.
//! The main function of the lexer is just to make sure the input is correctly spelled and
//! consists of valid words or inputs.

const std = @import("std");

const mvzr = @import("mvzr");

const Parser = @import("parser.zig").Parser;

const TokenType = enum {
    /// Assembler directives
    directive,
    /// Instruction
    instruction,
    /// Numeric literal
    literal,
    /// Register argument
    register,
    /// Unique identifier
    identifier,
    /// End of line
    newline,
    /// Various operators
    operator,
    /// End of file
    eof,
    /// Invalid token
    syntax_error,
    /// Invalid literal, i.e. does not fit into an i32
    value_error,
};

pub const Directive = enum(u8) {
    constant,
    variable,
    array,
    macro_start,
    macro_end,
    /// Not an actual directive, just tells how long the enum is
    len,
};

const Directives = [_][]const u8{
    "const",
    "var",
    "array",
    "macro",
    "endm",
};

pub const Operator = enum {
    label,
    immediate,
    macro_argument,
    list_start,
    list_end,
    comma,
};

/// Instruction divided into its various possible components
pub const Instruction = struct {
    operation: InstructionEnum,
    /// For ldr/str: ib, ia, db, da
    suffix: ?InstructionSuffix,
    /// s or l
    flag: bool,
    /// Optional two letter condition code
    condition: ConditionCode,
};

pub const InstructionEnum = enum(u8) {
    i_setf,
    i_wait,
    i_push,
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
    i_mov,
    i_cmp,
    i_not,
    i_ldr,
    i_str,
    i_ldh,
    i_pop,
    i_brk,
    i_b,
};

const Instructions = [_][]const u8{
    "setf",
    "wait",
    "push",
    "nop",
    "add",
    "sub",
    "sbn",
    "mul",
    "div",
    "mod",
    "exp",
    "shl",
    "shr",
    "and",
    "orr",
    "xor",
    "mov",
    "cmp",
    "not",
    "ldr",
    "str",
    "ldh",
    "pop",
    "brk",
    "b",
};

pub const InstructionSuffix = enum(u8) {
    ia,
    ib,
    da,
    db,
};

const InstructionSuffixes = [_][]const u8{
    "ia",
    "ib",
    "da",
    "db",
};

pub const ConditionCode = enum(u4) {
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

const ConditionCodes = [_][]const u8{
    "al",
    "eq",
    "ne",
    "ng",
    "pz",
    "lt",
    "le",
    "gt",
    "ge",
    "vs",
    "vc",
    "es",
    "ec",
};

pub const Token = struct {
    type: TokenType,
    value: i32 = 0,
    slice: []const u8,
    instruction: ?Instruction = null,
    directive: ?Directive = null,
};

pub const Lexer = struct {
    input: []u8,
    regexes: std.AutoHashMap(TokenType, mvzr.Regex) = undefined,

    /// Used for identifier lookup
    parser: *Parser,

    pub fn init(self: *Lexer, allocator: std.mem.Allocator) !void {
        self.regexes = std.AutoHashMap(TokenType, mvzr.Regex).init(allocator);

        try self.regexes.put(.newline, mvzr.compile("^\n").?);
        try self.regexes.put(.directive, mvzr.compile("^@\\w+").?);
        try self.regexes.put(.operator, mvzr.compile("^[\\.#\\$\\[\\]\\,]").?);
        try self.regexes.put(.register, mvzr.compile("^(x[0-9]\\b)|(x1[012345]\\b)|(t[12]\\b)|(sp\\b)|(lr\\b)|(pc\\b)").?);
        try self.regexes.put(.literal, mvzr.compile("^[+-]?((0x[0-9abcdef]+\\b)|(0b[01]+\\b)|(0o[0-7]+\\b)|([0-9]+\\b))").?);
        try self.regexes.put(.instruction, mvzr.compile("^\\w+").?);
        try self.regexes.put(.identifier, mvzr.compile("^[a-zA-Z_][a-zA-Z_0-9]*").?);
        try self.regexes.put(.syntax_error, mvzr.compile("^[^\n]+").?);
    }

    pub fn deinit(self: *Lexer) void {
        self.regexes.deinit();
    }

    /// Consume a token and return it
    pub fn lex(self: *Lexer) Token {
        const ret: Token = self.look();
        self.input = self.input[ret.slice.len..];
        return ret;
    }

    /// Look ahead one token and return it without consuming it
    pub fn look(self: *Lexer) Token {
        // Consume all whitespace first, it is never relevant except to separate tokens
        while (self.input.len > 0 and (self.input[0] == ' ' or self.input[0] == '\t')) {
            self.input = self.input[1..];
        }

        // Discard comments
        if (self.input.len > 0 and self.input[0] == ';') {
            self.input = self.input[self.regexes.get(.syntax_error).?.match(self.input).?.slice.len..];
        }

        // If at the end, always return EOF
        if (self.input.len == 0)
            return .{ .type = .eof, .slice = &.{} };

        // Run through all regex patterns and return first matching one as a token

        const token_types = [_]TokenType{ .newline, .directive, .operator, .register, .literal, .instruction, .identifier, .syntax_error };

        var return_token: Token = .{ .type = .syntax_error, .slice = &.{} };
        for (token_types) |tt| {
            // Note: regex.match returns an optional which we unwrap with the if statement
            if (self.regexes.get(tt).?.match(self.input)) |m| {
                return_token.type = tt;
                return_token.slice = m.slice;

                // Since instructions are just like identifiers, try parse the instruction before declaring it one
                if (tt == .instruction) {
                    return_token.instruction = self.parseInstruction(return_token.slice);
                    if (return_token.instruction == null) continue;
                }

                break;
            }
        }

        // Set values depending on token type. Only does context-free parsing, e.g. getting the instruction and its components, not e.g. its format or the number of arguments
        switch (return_token.type) {
            .register => {
                return_token.value = self.parseRegister(return_token.slice);
            },
            .literal => {
                return_token.value = std.fmt.parseInt(i32, return_token.slice, 0) catch blk: {
                    const u32val = std.fmt.parseInt(u32, return_token.slice, 0) catch blk2: {
                        return_token.type = .value_error;
                        break :blk2 0;
                    };
                    break :blk @bitCast(u32val);
                };
            },
            .directive => {
                return_token.directive = self.parseDirective(return_token.slice);
                if (return_token.directive == null) {
                    return_token.type = .syntax_error;
                }
            },
            .identifier => {
                const symbol = self.parser.symbols.get(return_token.slice);
                if (symbol != null) return_token.value = symbol.?.value;
            },
            else => {},
        }

        return return_token;
    }

    fn parseRegister(self: *Lexer, slice: []const u8) u4 {
        _ = self;
        var value: u4 = 0;
        if (slice[0] == 'x') {
            for (slice[1..]) |c| {
                value *= 10;
                value += @truncate(c - '0');
            }
            return value;
        } else if (slice[0] == 't') {
            return if (slice[1] == '1') 11 else 12;
        } else if (std.meta.eql(slice, "sp")) {
            return 13;
        } else if (std.meta.eql(slice, "lr")) {
            return 14;
        } else if (std.meta.eql(slice, "pc")) {
            return 15;
        }
        unreachable;
    }

    fn parseInstruction(self: *Lexer, slice: []const u8) ?Instruction {
        _ = self;
        var ret = Instruction{ .operation = .i_nop, .condition = .al, .flag = false, .suffix = null };
        var len: usize = 0;
        for (Instructions, 0..) |ins, i| {
            // Instruction proper
            if (begins(slice, ins)) {
                ret.operation = @enumFromInt(i);
                len += ins.len;
            } else continue;

            // Suffix
            if (std.mem.eql(u8, ins, "ldr") or std.mem.eql(u8, ins, "str")) {
                for (InstructionSuffixes, 0..) |suffix, j| {
                    if (begins(slice[len..], suffix)) {
                        ret.suffix = @enumFromInt(j);
                        len += suffix.len;
                        break;
                    }
                }
            }

            // Flag
            if (slice.len > len and (slice[len] == 's' or std.mem.eql(u8, ins, "b") and slice[len] == 'l' and slice.len != (len + 2))) {
                ret.flag = true;
                len += 1;
            }

            // Condition code
            for (ConditionCodes, 0..) |cond, j| {
                if (std.mem.eql(u8, slice[len..], cond)) {
                    ret.condition = @enumFromInt(j);
                    len += cond.len;
                    break;
                }
            }

            if (len == slice.len) return ret;
        }
        return null;
    }

    fn parseDirective(self: *Lexer, slice: []const u8) ?Directive {
        _ = self;
        for (0..@intFromEnum(Directive.len)) |i| {
            if (std.mem.eql(u8, slice[1..], Directives[i])) {
                return @enumFromInt(i);
            }
        }
        return null;
    }
};

fn begins(string: []const u8, start: []const u8) bool {
    return string.len >= start.len and std.mem.eql(u8, start, string[0..start.len]);
}
