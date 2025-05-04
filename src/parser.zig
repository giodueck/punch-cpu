const std = @import("std");

const lexer = @import("lexer.zig");
const Lexer = lexer.Lexer;
const Token = lexer.Token;
const Directive = lexer.Directive;
const Instruction = lexer.Instruction;

const stderr = std.io.getStdErr().writer();

const Argument = enum {
    register,
    immediate,
    imm_ident,
};

const SymbolType = enum {
    constant,
    variable,
    array,
    macro,
    label,
};

const Symbol = struct {
    type: SymbolType,
    identifier: []const u8,
    /// Constant value, label instruction index, variable/array address
    value: usize,
    /// Argument count for macros
    arg_count: usize,
};

pub const Parser = struct {
    lexer: Lexer = undefined,
    input: []u8,
    lines: std.ArrayList([]const u8) = undefined,
    curr_line: usize = 1,
    err_buffer: [512]u8 = [_]u8{8} ** 512,
    err_count: usize = 0,
    symbols: std.AutoHashMap([]const u8, Symbol) = undefined,

    pub fn init(self: *Parser, allocator: std.mem.Allocator) !void {
        self.lexer = Lexer{ .input = self.input };
        try self.lexer.init(allocator);

        self.lines = std.ArrayList([]const u8).init(allocator);
        var lines_iter = std.mem.splitScalar(u8, self.input, '\n');
        while (lines_iter.next()) |line| {
            try self.lines.append(line);
        }

        self.symbols = std.AutoHashMap([]const u8, Symbol).init(allocator);
    }

    pub fn deinit(self: *Parser) void {
        self.symbols.deinit();
        self.lines.deinit();
        self.lexer.deinit();
    }

    // TODO separate into stages, as in notes.md
    pub fn parse(self: *Parser) !void {
        var token = self.lexer.lex();
        while (token.type != .eof) : (token = self.lexer.lex()) {
            switch (token.type) {
                .directive => {
                    try self.parseDirective(token.directive.?);
                },
                .instruction => {
                    try self.parseInstruction(token.instruction.?);
                },
                .newline => {
                    try self.parseNewline();
                },
                .operator => {
                    switch (token.slice[0]) {
                        '.' => {
                            try self.parseLabel();
                        },
                        else => {
                            std.debug.print("Operator not implemented: {s}\n", .{token.slice});
                        },
                    }
                },
                else => {
                    std.debug.print("{any}: {s}\n", .{ token.type, token.slice });
                    try self.parseError(token, "not implemented");
                },
            }
        }
    }

    /// Panic: consume all tokens until end of line to make parsing the rest of the file possible
    /// Consumes all tokens up to and including the next newline, and increments the line count.
    /// If EOF is encountered, returns immediately without incrementing the line count.
    fn parseError(self: *Parser, last: Token, msg: []const u8) !void {
        var t = last;
        if (last.type != .syntax_error) {
            self.err_count += 1;
            try stderr.print("Error on line {d}: {s}\n\n", .{ self.curr_line, msg });
        }
        while (t.type != .newline) {
            if (t.type == .eof) {
                return;
            }

            if (t.type == .syntax_error) {
                self.err_count += 1;
                try stderr.print("Syntax error on line {d}: '{s}' in '{s}'\n\n", .{ self.curr_line, t.slice, self.lines.items[self.curr_line - 1] });
            }
            t = self.lexer.lex();
        }
        return self.parseNewline();
    }

    fn parseNewline(self: *Parser) !void {
        std.debug.print("HANDLE NEWLINE\n", .{});
        self.curr_line += 1;
    }

    fn parseDirective(self: *Parser, directive: Directive) !void {
        switch (directive) {
            // Constant format: "@const" identifier literal
            .constant => {
                const ident = self.lexer.lex();
                if (ident.type != .identifier) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected identifier, got '{s}'", .{ident.slice});
                    return self.parseError(ident, err_msg);
                }

                const literal = self.lexer.lex();
                if (literal.type != .literal) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected numeric literal, got '{s}'", .{literal.slice});
                    return self.parseError(literal, err_msg);
                }
                // TODO do something with the correct input
            },
            else => {
                std.debug.print("Not implemented: {}\n", .{directive});
            },
        }
    }

    fn parseInstruction(self: *Parser, instruction: Instruction) !void {
        _ = self;
        std.debug.print("{any} \n", .{instruction});
    }

    fn parseLabel(self: *Parser) !void {
        const ident = self.lexer.lex();
        if (ident.type != .identifier) {
            const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected identifier, got '{s}'", .{ident.slice});
            return self.parseError(ident, err_msg);
        }
        // TODO save identifier and instruction
        std.debug.print("Label with ident: {s}\n", .{ident.slice});
    }
};
