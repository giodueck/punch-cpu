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
    /// Constant value, label instruction index, variable/array address
    value: isize = 0,
    /// Argument count for macros
    arg_count: usize = 0,
};

const ParsedInstruction = struct {
    instruction: Instruction,
    destination: u4,
    op1: u4,
    op2: i32,
    immediate: bool,
};

pub const Parser = struct {
    lexer: Lexer = undefined,
    input: []u8,
    lines: std.ArrayList([]const u8) = undefined,
    curr_line: usize = 1,
    err_buffer: [512]u8 = [_]u8{8} ** 512,
    err_count: usize = 0,
    symbols: std.StringHashMap(Symbol) = undefined,
    program: std.ArrayList(ParsedInstruction) = undefined,
    data: std.ArrayList(i32) = undefined,

    pub fn init(self: *Parser, allocator: std.mem.Allocator) !void {
        self.lexer = Lexer{ .input = self.input };
        try self.lexer.init(allocator);

        self.lines = std.ArrayList([]const u8).init(allocator);
        var lines_iter = std.mem.splitScalar(u8, self.input, '\n');
        while (lines_iter.next()) |line| {
            try self.lines.append(line);
        }

        self.symbols = std.StringHashMap(Symbol).init(allocator);

        self.program = std.ArrayList(ParsedInstruction).init(allocator);
        self.data = std.ArrayList(i32).init(allocator);
    }

    pub fn deinit(self: *Parser) void {
        self.data.deinit();
        self.program.deinit();
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

        var iter = self.symbols.iterator();
        std.debug.print("\n\nSymbols:\n", .{});
        while (iter.next()) |s| {
            std.debug.print("{s}: {}\n", .{ s.key_ptr.*, s.value_ptr });
        }

        std.debug.print("\nData:\n", .{});
        for (self.data.items) |item| {
            std.debug.print("{d}, ", .{item});
        }
        std.debug.print("\n", .{});
    }

    /// Panic: consume all tokens until end of line to make parsing the rest of the file possible
    /// Consumes all tokens up to and including the next newline, and increments the line count.
    /// If EOF is encountered, returns immediately without incrementing the line count.
    fn parseError(self: *Parser, last: Token, msg: []const u8) !void {
        var t = last;
        if (last.type != .syntax_error and last.type != .value_error) {
            self.err_count += 1;
            try stderr.print("Error on line {d}: {s}\n", .{ self.curr_line, msg });
        } else if (last.type == .value_error) {
            self.err_count += 1;
            try stderr.print("Value error on line {d}: '{s}' does not fit in a 32-bit signed integer\n", .{ self.curr_line, last.slice });
        }
        while (t.type != .newline) {
            if (t.type == .eof) {
                return;
            }

            if (t.type == .syntax_error) {
                self.err_count += 1;
                try stderr.print("Syntax error on line {d}: '{s}' in '{s}'\n", .{ self.curr_line, t.slice, self.lines.items[self.curr_line - 1] });
            }
            t = self.lexer.lex();
        }
        return self.parseNewline();
    }

    fn parseNewline(self: *Parser) !void {
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

                // Save constant in symbol table (if the symbol is not defined yet)
                if (self.symbols.contains(ident.slice)) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "name '{s}' already defined", .{ident.slice});
                    return self.parseError(literal, err_msg);
                }
                try self.symbols.put(ident.slice, Symbol{ .type = .constant, .value = literal.value });
            },
            // Variable format: "@var" identifier literal
            .variable => {
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

                // Save constant in symbol table (if the symbol is not defined yet)
                if (self.symbols.contains(ident.slice)) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "name '{s}' already defined", .{ident.slice});
                    return self.parseError(literal, err_msg);
                }
                // Save to data arraylist first, then point to the last element
                const addr: i32 = @intCast(self.data.items.len);
                try self.data.append(literal.value);
                try self.symbols.put(ident.slice, Symbol{ .type = .variable, .value = addr });
            },
            // Array format: "@array" identifier literal [ list_of_literals ]
            // A list too long is an error, a list too short will be padded with 0s
            .array => {
                const ident = self.lexer.lex();
                if (ident.type != .identifier) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected identifier, got '{s}'", .{ident.slice});
                    return self.parseError(ident, err_msg);
                }

                // Check if symbol can be defined before going through all the items
                if (self.symbols.contains(ident.slice)) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "name '{s}' already defined", .{ident.slice});
                    return self.parseError(ident, err_msg);
                }
                const addr: i32 = @intCast(self.data.items.len);

                // Size of array
                const size = self.lexer.lex();
                if (size.type != .literal) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected numeric literal for size of array, got '{s}'", .{size.slice});
                    return self.parseError(size, err_msg);
                }

                // List start
                const list_start = self.lexer.lex();
                if (list_start.type != .operator or list_start.slice[0] != '[') {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected '[', got '{s}'", .{list_start.slice});
                    return self.parseError(list_start, err_msg);
                }

                // list_of_literals: literal
                //                 | list_of_literals "," literal
                //                 ;
                var i: i32 = 0;
                var list_ended = false;
                var tok: Token = self.lexer.lex();
                while (i < size.value) : (i += 1) {
                    if (!list_ended) {
                        if (tok.type != .literal) {
                            const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected numeric literal, got '{s}'", .{tok.slice});
                            return self.parseError(tok, err_msg);
                        }

                        // Store value
                        try self.data.append(tok.value);

                        // Next could be either a comma (list continues) or a ']' (list ended)
                        // If (i == size.value - 1), however, no more values should appear
                        tok = self.lexer.look();

                        if (tok.type != .operator or (tok.slice[0] != ',' and tok.slice[0] != ']')) {
                            const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected ',' or ']', got '{s}'", .{tok.slice});
                            return self.parseError(tok, err_msg);
                        } else if (tok.type == .operator and tok.slice[0] == ',') {
                            if (i == size.value - 1) {
                                const err_msg = try std.fmt.bufPrint(&self.err_buffer, "unexpected trailing ',' or too many values, expected only {d}", .{size.value});
                                tok = self.lexer.lex(); // consume comma to give to parseError
                                return self.parseError(tok, err_msg);
                            }
                            tok = self.lexer.lex(); // consume comma
                            tok = self.lexer.lex(); // get next literal
                        } else if (tok.type == .operator and tok.slice[0] == ']') {
                            list_ended = true; // don't consume any more tokens
                        }
                    } else {
                        // Store a zero for unused space
                        try self.data.append(tok.value);
                    }
                }

                // List end
                const list_end = self.lexer.lex();
                if (list_end.type != .operator or list_end.slice[0] != ']') {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected ']', got '{s}'", .{list_end.slice});
                    return self.parseError(list_end, err_msg);
                }

                // Finally store symbol
                try self.symbols.put(ident.slice, Symbol{ .type = .array, .value = addr });
            },
            // TODO macro
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
