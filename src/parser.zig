const std = @import("std");

const lexer = @import("lexer.zig");
const Lexer = lexer.Lexer;
const Token = lexer.Token;
const Directive = lexer.Directive;
const Instruction = lexer.Instruction;
const Operator = lexer.Operator;

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
    /// Constant value, label instruction index, variable/array address, macro index
    value: i32 = 0,
};

const ParsedInstruction = struct {
    instruction: Instruction,
    destination: u4,
    op1: u4,
    op2: i32,
    immediate: bool,
    e_bits: u8 = 0,
};

const MacroInstruction = struct {
    instruction: Instruction,
    arg1op: ?Operator = null,
    arg1: ?Token = null,
    arg2op: ?Operator = null,
    arg2: ?Token = null,
    arg3op: ?Operator = null,
    arg3: ?Token = null,
};

const Macro = struct {
    arg_count: usize = 0,
    instructions: std.ArrayList(MacroInstruction) = undefined,
};

pub const Parser = struct {
    allocator: std.mem.Allocator = undefined,
    lexer: Lexer = undefined,
    input: []u8,

    lines: std.ArrayList([]const u8) = undefined,
    curr_line: usize = 1,
    /// Whether the parser is currently parsing a macro definition
    in_macro: bool = false,
    /// Whether the parser is currently parsing a macro call, and which one
    curr_macro: ?[]const u8 = null,

    err_buffer: [512]u8 = [_]u8{8} ** 512,
    err_count: u32 = 0,

    symbols: std.StringHashMap(Symbol) = undefined,
    macros: std.ArrayList(Macro) = undefined,
    program: std.ArrayList(ParsedInstruction) = undefined,
    data: std.ArrayList(i32) = undefined,

    pub fn init(self: *Parser, allocator: std.mem.Allocator) !void {
        self.allocator = allocator;

        self.lexer = Lexer{ .input = self.input, .parser = self };
        try self.lexer.init(allocator);

        self.lines = std.ArrayList([]const u8).init(allocator);
        var lines_iter = std.mem.splitScalar(u8, self.input, '\n');
        while (lines_iter.next()) |line| {
            try self.lines.append(line);
        }

        self.symbols = std.StringHashMap(Symbol).init(allocator);
        self.macros = std.ArrayList(Macro).init(allocator);
        self.program = std.ArrayList(ParsedInstruction).init(allocator);
        self.data = std.ArrayList(i32).init(allocator);
    }

    pub fn deinit(self: *Parser) void {
        self.data.deinit();
        self.program.deinit();
        for (self.macros.items) |m| {
            m.instructions.deinit();
        }
        self.macros.deinit();
        self.symbols.deinit();
        self.lines.deinit();
        self.lexer.deinit();
    }

    // TODO separate into stages, as in notes.md
    pub fn parse(self: *Parser) !u32 {
        var token = self.lexer.lex();
        var expect_eol = false;

        // First pass

        // Take one token and run a parser rule based on that token, except when the last rule expected an end of line
        while (token.type != .eof) : (token = self.lexer.lex()) {
            if (expect_eol) {
                if (token.type != .newline) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected end of line, got '{s}'", .{token.slice});
                    try self.parseError(token, err_msg);
                } else {
                    try self.parseNewline();
                }
                expect_eol = false;
            } else {
                switch (token.type) {
                    .directive => {
                        try self.parseDirective(token.directive.?);
                        expect_eol = true;
                    },
                    .instruction => {
                        const ins = try self.parseInstruction(token.instruction.?);
                        if (ins == null) {
                            // ignore, errors should already be reported
                            continue;
                        }
                        try self.program.append(ins.?);
                        expect_eol = true;
                    },
                    .newline => {
                        try self.parseNewline();
                    },
                    .operator => {
                        switch (token.slice[0]) {
                            '.' => {
                                try self.parseLabelFirst();
                            },
                            else => {
                                try stderr.print("Operator not implemented: {s}\n", .{token.slice});
                            },
                        }
                    },
                    .identifier => {
                        // This can only be a macro
                        const instrs = try self.resolveMacro(token);
                        if (instrs != null) {
                            defer instrs.?.deinit();
                            for (instrs.?.items) |ins| {
                                try self.program.append(ins);
                            }
                        } // else ignore, errors should already be reported
                    },
                    else => {
                        std.debug.print("{any}: {s}\n", .{ token.type, token.slice });
                        try self.parseError(token, "not implemented");
                    },
                }
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

        std.debug.print("\nMacros:\n", .{});
        for (self.macros.items) |m| {
            std.debug.print("{} {any}\n", .{ m.arg_count, m.instructions.items });
        }
        std.debug.print("\n", .{});

        std.debug.print("\nProgram:\n", .{});
        for (self.program.items) |ins| {
            std.debug.print("{}\n", .{ins});
        }
        std.debug.print("\n", .{});

        return self.err_count;
    }

    /// Panic: consume all tokens until end of line to make parsing the rest of the file possible
    /// Consumes all tokens up to and including the next newline, and increments the line count.
    /// If EOF is encountered, returns immediately without incrementing the line count.
    fn parseError(self: *Parser, last: Token, msg: []const u8) !void {
        var t = last;
        if (last.type != .syntax_error and last.type != .value_error) {
            self.err_count += 1;
            try stderr.print("Error on line {d}: {s}\n", .{ self.curr_line, msg });
            if (self.curr_macro != null) {
                try stderr.print("  in macro '{s}'\n", .{self.curr_macro.?});
            }

            // If currently in a macro, skip the rest of it
            if (self.in_macro) {
                var tok = self.lexer.lex();
                while (tok.type != .directive or tok.directive.? != .macro_end) {
                    if (tok.type == .newline) try self.parseNewline();
                    if (tok.type == .eof) return;
                    tok = self.lexer.lex();
                }

                self.in_macro = false;
                return;
            }
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
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected identifier, got '{s}'", .{if (ident.slice[0] != '\n') ident.slice else "\\n"});
                    return self.parseError(ident, err_msg);
                }

                const literal = self.lexer.lex();
                if (literal.type != .literal) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected numeric literal, got '{s}'", .{if (literal.slice[0] != '\n') literal.slice else "\\n"});
                    return self.parseError(literal, err_msg);
                }

                // Save constant in symbol table (if the symbol is not defined yet)
                if (self.symbols.contains(ident.slice)) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "name '{s}' already defined", .{if (ident.slice[0] != '\n') ident.slice else "\\n"});
                    return self.parseError(literal, err_msg);
                }
                try self.symbols.put(ident.slice, Symbol{ .type = .constant, .value = literal.value });
            },
            // Variable format: "@var" identifier literal
            .variable => {
                const ident = self.lexer.lex();
                if (ident.type != .identifier) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected identifier, got '{s}'", .{if (ident.slice[0] != '\n') ident.slice else "\\n"});
                    return self.parseError(ident, err_msg);
                }

                const literal = self.lexer.lex();
                if (literal.type != .literal) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected numeric literal, got '{s}'", .{if (literal.slice[0] != '\n') literal.slice else "\\n"});
                    return self.parseError(literal, err_msg);
                }

                // Save constant in symbol table (if the symbol is not defined yet)
                if (self.symbols.contains(ident.slice)) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "name '{s}' already defined", .{if (ident.slice[0] != '\n') ident.slice else "\\n"});
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
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected identifier, got '{s}'", .{if (ident.slice[0] != '\n') ident.slice else "\\n"});
                    return self.parseError(ident, err_msg);
                }

                // Check if symbol can be defined before going through all the items
                if (self.symbols.contains(ident.slice)) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "name '{s}' already defined", .{if (ident.slice[0] != '\n') ident.slice else "\\n"});
                    return self.parseError(ident, err_msg);
                }
                const addr: i32 = @intCast(self.data.items.len);

                // Size of array
                const size = self.lexer.lex();
                if (size.type != .literal) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected numeric literal for size of array, got '{s}'", .{if (size.slice[0] != '\n') size.slice else "\\n"});
                    return self.parseError(size, err_msg);
                }

                // List start
                const list_start = self.lexer.lex();
                if (list_start.type != .operator or list_start.slice[0] != '[') {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected '[', got '{s}'", .{if (list_start.slice[0] != '\n') list_start.slice else "\\n"});
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
                            const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected numeric literal, got '{s}'", .{if (tok.slice[0] != '\n') tok.slice else "\\n"});
                            return self.parseError(tok, err_msg);
                        }

                        // Store value
                        try self.data.append(tok.value);

                        // Next could be either a comma (list continues) or a ']' (list ended)
                        // If (i == size.value - 1), however, no more values should appear
                        tok = self.lexer.look();

                        if (tok.type != .operator or (tok.slice[0] != ',' and tok.slice[0] != ']')) {
                            const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected ',' or ']', got '{s}'", .{if (tok.slice[0] != '\n') tok.slice else "\\n"});
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
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected ']', got '{s}'", .{if (list_end.slice[0] != '\n') list_end.slice else "\\n"});
                    return self.parseError(list_end, err_msg);
                }

                // Finally store symbol
                try self.symbols.put(ident.slice, Symbol{ .type = .array, .value = addr });
            },
            // Macro format: "@macro" ident literal
            // While in macro, should only accept intructions or macro calls
            .macro_start => {
                self.in_macro = true;
                const ident = self.lexer.lex();
                if (ident.type != .identifier) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected identifier, got '{s}'", .{if (ident.slice[0] != '\n') ident.slice else "\\n"});
                    return self.parseError(ident, err_msg);
                }

                // Check if symbol can be defined before going through the rest
                if (self.symbols.contains(ident.slice)) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "name '{s}' already defined", .{if (ident.slice[0] != '\n') ident.slice else "\\n"});
                    return self.parseError(ident, err_msg);
                }

                // Number of arguments
                const literal = self.lexer.lex();
                if (literal.type != .literal) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected numeric literal, got '{s}'", .{if (literal.slice[0] != '\n') literal.slice else "\\n"});
                    return self.parseError(literal, err_msg);
                }

                if (literal.value < 0) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected non-negative literal for macro argument count, got '{s}'", .{if (literal.slice[0] != '\n') literal.slice else "\\n"});
                    return self.parseError(literal, err_msg);
                }

                const newline = self.lexer.lex();
                if (newline.type != .newline) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected end of line, got '{s}'", .{if (literal.slice[0] != '\n') literal.slice else "\\n"});
                    return self.parseError(newline, err_msg);
                }
                try self.parseNewline();

                var macro: Macro = .{ .instructions = std.ArrayList(MacroInstruction).init(self.allocator), .arg_count = @intCast(literal.value) };

                // TODO support nested macros. Assume right now we can only get instructions
                var tok = self.lexer.look();
                while (tok.type == .instruction) : (tok = self.lexer.look()) {
                    try self.parseMacroInstruction(&macro);
                }

                // If some instruction ran into an error, it would call parseError, which panic discards the
                // rest of the macro. Check for that condition
                if (!self.in_macro) {
                    macro.instructions.deinit();
                    return;
                }

                // Broke out of loop because lexer gave something not allowed inside macro definition, check if this is a @endm
                if (tok.type != .directive or tok.directive.? != .macro_end) {
                    tok = self.lexer.lex();
                    macro.instructions.deinit();
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "unexpected '{s}', macro definitions may only contain instructions", .{if (tok.slice[0] != '\n') tok.slice else "\\n"});
                    return self.parseError(tok, err_msg);
                }
                // Macro end format: "@endm"
                _ = self.lexer.lex();

                self.in_macro = false;
                try self.symbols.put(ident.slice, Symbol{ .type = .macro, .value = @intCast(self.macros.items.len) });
                try self.macros.append(macro);
            },
            else => {
                std.debug.print("unexpected directive {}\n", .{directive});
            },
        }
    }

    /// Parses instruction and its arguments
    fn parseInstruction(self: *Parser, instruction: Instruction) !?ParsedInstruction {
        // Get args
        var args_array = std.ArrayList(Token).init(self.allocator);
        defer args_array.deinit();
        const args = &args_array.items;

        // Don't want to consume the newline
        var tok = self.lexer.look();
        var op: ?Operator = null;
        while (true) : (tok = self.lexer.look()) {
            switch (tok.type) {
                .newline, .eof => {
                    break;
                },
                .register => {
                    try args_array.append(tok);
                    // consume token
                    _ = self.lexer.lex();
                },
                .operator => {
                    // consume token
                    _ = self.lexer.lex();
                    if (tok.slice[0] == '#') {
                        op = .immediate;
                        continue;
                    } else {
                        const err_msg = try std.fmt.bufPrint(&self.err_buffer, "unexpected operator: '{s}'", .{tok.slice});
                        try self.parseError(tok, err_msg);
                        return null;
                    }
                },
                .literal => {
                    // consume token
                    _ = self.lexer.lex();
                    if (op == null) {
                        const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected immediate operator '#' before immediate", .{});
                        try self.parseError(tok, err_msg);
                        return null;
                    }

                    try args_array.append(tok);
                },
                .identifier => {
                    if (self.symbols.get(tok.slice)) |sym| {
                        if (sym.type == .macro) {
                            const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected constant, variable, array or label, but got macro '{s}'", .{tok.slice});
                            try self.parseError(tok, err_msg);
                            return null;
                        }
                        try args_array.append(tok);
                    } else {
                        const err_msg = try std.fmt.bufPrint(&self.err_buffer, "symbol '{s}' is undefined", .{tok.slice});
                        try self.parseError(tok, err_msg);
                        return null;
                    }
                },
                else => {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected register, immediate or idendifier, got '{s}'", .{tok.slice});
                    try self.parseError(tok, err_msg);
                    return null;
                },
            }
            op = null;
        }

        if (args.len > 3) {
            const err_msg = try std.fmt.bufPrint(&self.err_buffer, "too many arguments", .{});
            try self.parseError(tok, err_msg);
            return null;
        }

        return self.assembleInstruction(instruction, if (args.len > 0) args.*[0] else null, if (args.len > 1) args.*[1] else null, if (args.len > 2) args.*[2] else null);
    }

    /// Parses the instruction as part of a macro: takes either register, immediate, or macro parameter arguments
    /// Assumes no tokens have been consumed yet, i.e. the next token to be consumed by the lexer is the instruction
    fn parseMacroInstruction(self: *Parser, macro: *Macro) !void {
        const ins = self.lexer.lex();
        if (ins.type != .instruction) {
            const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected macro instruction, got '{s}'", .{if (ins.slice[0] != '\n') ins.slice else "\\n"});
            return self.parseError(ins, err_msg);
        }
        var instr = MacroInstruction{ .instruction = ins.instruction.? };

        // Longest instructions take 3 arguments
        for (0..3) |i| {
            var tok = self.lexer.look();
            if (tok.type == .newline) break;
            tok = self.lexer.lex();
            // We do almost no parsing or checking here, that can come when actually parsing the instructions generated when calling the macro
            switch (i) {
                0 => {
                    if (tok.type == .operator) {
                        instr.arg1op = switch (tok.slice[0]) {
                            '#' => .immediate,
                            '$' => .macro_argument,
                            else => .comma,
                        };
                        if (instr.arg1op == .comma) {
                            const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected macro argument, got '{s}'", .{if (ins.slice[0] != '\n') ins.slice else "\\n"});
                            return self.parseError(tok, err_msg);
                        }
                        tok = self.lexer.lex();
                    }
                    if (tok.type == .identifier and self.symbols.get(tok.slice) != null) {
                        if (self.symbols.get(tok.slice).?.type == .macro) {
                            const err_msg = try std.fmt.bufPrint(&self.err_buffer, "invalid argument, got '{s}' which is a macro", .{if (tok.slice[0] != '\n') tok.slice else "\\n"});
                            return self.parseError(tok, err_msg);
                        }
                    }
                    instr.arg1 = tok;
                },
                1 => {
                    if (tok.type == .operator) {
                        instr.arg2op = switch (tok.slice[0]) {
                            '#' => .immediate,
                            '$' => .macro_argument,
                            else => .comma,
                        };
                        if (instr.arg2op == .comma) {
                            const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected macro argument, got '{s}'", .{if (ins.slice[0] != '\n') ins.slice else "\\n"});
                            return self.parseError(tok, err_msg);
                        }
                        tok = self.lexer.lex();
                    }
                    if (tok.type == .identifier and self.symbols.get(tok.slice) != null) {
                        if (self.symbols.get(tok.slice).?.type == .macro) {
                            const err_msg = try std.fmt.bufPrint(&self.err_buffer, "invalid argument, got '{s}' which is a macro", .{if (tok.slice[0] != '\n') tok.slice else "\\n"});
                            return self.parseError(tok, err_msg);
                        }
                    }
                    instr.arg2 = tok;
                },
                2 => {
                    if (tok.type == .operator) {
                        instr.arg3op = switch (tok.slice[0]) {
                            '#' => .immediate,
                            '$' => .macro_argument,
                            else => .comma,
                        };
                        if (instr.arg3op == .comma) {
                            const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected macro argument, got '{s}'", .{if (ins.slice[0] != '\n') ins.slice else "\\n"});
                            return self.parseError(tok, err_msg);
                        }
                        tok = self.lexer.lex();
                    }
                    if (tok.type == .identifier and self.symbols.get(tok.slice) != null) {
                        if (self.symbols.get(tok.slice).?.type == .macro) {
                            const err_msg = try std.fmt.bufPrint(&self.err_buffer, "invalid argument, got '{s}' which is a macro", .{if (tok.slice[0] != '\n') tok.slice else "\\n"});
                            return self.parseError(tok, err_msg);
                        }
                    }
                    instr.arg3 = tok;
                },
                else => unreachable,
            }
        }

        // Now we should only get a newline
        const tok = self.lexer.lex();
        if (tok.type != .newline) {
            const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expexted end of line, got '{s}'", .{ins.slice});
            return self.parseError(tok, err_msg);
        }
        try self.parseNewline();

        try macro.instructions.append(instr);
    }

    /// Macros are called with their identifier and a sequence of tokens matching in length to the number of arguments defined in the macro definition.
    /// The returned ArrayList needs to be deinitialized when it is returned.
    fn resolveMacro(self: *Parser, ident: Token) !?std.ArrayList(ParsedInstruction) {
        const macro_opt = self.symbols.get(ident.slice);
        if (macro_opt == null or macro_opt.?.type != .macro) {
            const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'{s}' is not a defined macro", .{ident.slice});
            try self.parseError(ident, err_msg);
            return null;
        }

        const macro = self.macros.items[@intCast(macro_opt.?.value)];
        self.curr_macro = ident.slice;

        // Get args
        var args_array = std.ArrayList(Token).init(self.allocator);
        defer args_array.deinit();
        const args = &args_array.items;

        // Don't want to consume the newline
        var tok = self.lexer.look();
        var op: ?Operator = null;
        while (true) : (tok = self.lexer.look()) {
            switch (tok.type) {
                .newline, .eof => {
                    break;
                },
                .register => {
                    try args_array.append(tok);
                    // consume token
                    _ = self.lexer.lex();
                },
                .operator => {
                    // consume token
                    _ = self.lexer.lex();
                    if (tok.slice[0] == '#') {
                        op = .immediate;
                        continue;
                    } else {
                        const err_msg = try std.fmt.bufPrint(&self.err_buffer, "unexpected operator: '{s}'", .{tok.slice});
                        try self.parseError(tok, err_msg);
                        return null;
                    }
                },
                .literal => {
                    // consume token
                    _ = self.lexer.lex();
                    if (op == null) {
                        const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected immediate operator '#' before immediate", .{});
                        try self.parseError(tok, err_msg);
                        return null;
                    }

                    try args_array.append(tok);
                },
                .identifier => {
                    // consume token
                    _ = self.lexer.lex();
                    if (self.symbols.get(tok.slice)) |sym| {
                        if (sym.type == .macro) {
                            const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected constant, variable, array or label, but got macro '{s}'", .{tok.slice});
                            try self.parseError(tok, err_msg);
                            return null;
                        }
                        try args_array.append(tok);
                    } else {
                        const err_msg = try std.fmt.bufPrint(&self.err_buffer, "symbol '{s}' is undefined", .{tok.slice});
                        try self.parseError(tok, err_msg);
                        return null;
                    }
                },
                else => {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected register, immediate or idendifier, got '{s}'", .{tok.slice});
                    try self.parseError(tok, err_msg);
                    return null;
                },
            }
            op = null;
        }

        if (args.len != macro.arg_count) {
            const err_msg = try std.fmt.bufPrint(&self.err_buffer, "incorrect number of arguments for macro '{s}'", .{ident.slice});
            try self.parseError(ident, err_msg);
            return null;
        }

        // Return array
        var instrs = std.ArrayList(ParsedInstruction).init(self.allocator);

        for (macro.instructions.items) |m_inst| {
            var arg1 = m_inst.arg1;
            if (arg1 != null and m_inst.arg1op != null) {
                if (m_inst.arg1op.? == .macro_argument) {
                    arg1 = args.*[@intCast(arg1.?.value)];
                }
            }

            var arg2 = m_inst.arg2;
            if (arg2 != null and m_inst.arg2op != null) {
                if (m_inst.arg2op.? == .macro_argument) {
                    arg2 = args.*[@intCast(arg2.?.value)];
                }
            }

            var arg3 = m_inst.arg3;
            if (arg3 != null and m_inst.arg3op != null) {
                if (m_inst.arg3op.? == .macro_argument) {
                    arg3 = args.*[@intCast(arg3.?.value)];
                }
            }

            const p_instr = try self.assembleInstruction(m_inst.instruction, arg1, arg2, arg3);
            if (p_instr == null) {
                instrs.deinit();
                return null;
            }
            try instrs.append(p_instr.?);
        }

        self.curr_macro = null;
        return instrs;
    }

    /// Assemble Instruction with arguments into a ParsedInstruction, or return null and report if there was an error.
    /// Assumes the next token to be consumed by the lexer is a newline or eof
    fn assembleInstruction(self: *Parser, instruction: Instruction, arg1: ?Token, arg2: ?Token, arg3: ?Token) !?ParsedInstruction {
        const dummy_token = Token{ .type = .instruction, .slice = &.{} };
        switch (instruction.operation) {
            .i_setf => {
                // setf xs/imm
                if (arg1 == null or arg2 != null) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'setf' takes 1 argument: xs/imm", .{});
                    try self.parseError(dummy_token, err_msg);
                    return null;
                }

                return ParsedInstruction{ .instruction = instruction, .destination = 0, .op1 = 0, .op2 = arg1.?.value, .immediate = (arg1.?.type != .literal) };
            },
            .i_wait => {
                // wait t1/t2
                if (arg1 == null or arg2 != null) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'wait' takes 1 argument: t1/t2", .{});
                    try self.parseError(dummy_token, err_msg);
                    return null;
                }

                // arg1 may only be a register, and only very specific registers: t1/t2, aka x11/x12
                if (arg1.?.type != .register or (arg1.?.value != 11 and arg1.?.value != 12)) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'wait' argument 1 must be one of t1, t2 (aka x11, x12)", .{});
                    try self.parseError(dummy_token, err_msg);
                    return null;
                }

                if (instruction.flag) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'{s}' cannot set flags", .{@tagName(instruction.operation)[2..]});
                    try self.parseError(dummy_token, err_msg);
                    return null;
                }

                return ParsedInstruction{ .instruction = instruction, .destination = 0, .op1 = 0, .op2 = 0, .immediate = false, .e_bits = @truncate(@as(u32, @intCast(arg1.?.value & 0xFF))) };
            },
            .i_nop, .i_brk => {
                if (arg1 != null) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'{s}' takes no arguments", .{@tagName(instruction.operation)[2..]});
                    try self.parseError(dummy_token, err_msg);
                    return null;
                }

                if (instruction.flag) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'{s}' cannot set flags", .{@tagName(instruction.operation)[2..]});
                    try self.parseError(dummy_token, err_msg);
                    return null;
                }

                return ParsedInstruction{ .instruction = instruction, .destination = 0, .op1 = 0, .op2 = 0, .immediate = false };
            },
            .i_add, .i_sub, .i_sbn, .i_mul, .i_div, .i_mod, .i_exp, .i_shl, .i_shr, .i_and, .i_orr, .i_xor => {
                if (arg1 == null or arg2 == null) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'{s}' takes 2 or 3 arguments: <xd> xr xs/imm", .{@tagName(instruction.operation)[2..]});
                    try self.parseError(dummy_token, err_msg);
                    return null;
                }

                if (arg3 != null) {
                    // alu xd xr xs/imm
                    if (arg1.?.type != .register) {
                        const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'{s}' argument 1 must be a register", .{@tagName(instruction.operation)[2..]});
                        try self.parseError(dummy_token, err_msg);
                        return null;
                    }

                    if (arg2.?.type != .register) {
                        const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'{s}' argument 2 must be a register when passing 3 arguments", .{@tagName(instruction.operation)[2..]});
                        try self.parseError(dummy_token, err_msg);
                        return null;
                    }

                    return ParsedInstruction{ .instruction = instruction, .destination = @truncate(@as(u32, @intCast(arg1.?.value & 0xF))), .op1 = @truncate(@as(u32, @intCast(arg2.?.value & 0xF))), .op2 = arg3.?.value, .immediate = (arg3.?.type != .register), .e_bits = 0 };
                } else {
                    // alu xr xs/imm
                    if (arg1.?.type != .register) {
                        const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'{s}' argument 1 must be a register", .{@tagName(instruction.operation)[2..]});
                        try self.parseError(dummy_token, err_msg);
                        return null;
                    }

                    return ParsedInstruction{ .instruction = instruction, .destination = @truncate(@as(u32, @intCast(arg1.?.value & 0xF))), .op1 = @truncate(@as(u32, @intCast(arg2.?.value & 0xF))), .op2 = arg2.?.value, .immediate = (arg2.?.type != .register), .e_bits = 0 };
                }
            },
            .i_ldr, .i_str => {
                // ldr/str xd xs/imm [v]
                if (arg1 == null or arg2 == null) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'{s}' takes 2 or 3 arguments: xd xs/imm [v]", .{@tagName(instruction.operation)[2..]});
                    try self.parseError(dummy_token, err_msg);
                    return null;
                }

                if (arg1.?.type != .register) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'{s}' argument 1 must be a register", .{@tagName(instruction.operation)[2..]});
                    try self.parseError(dummy_token, err_msg);
                    return null;
                }

                var e: u8 = 0;
                if (instruction.suffix != null) {
                    if (arg2.?.type != .register) {
                        const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'{s}{s}' address modification only supported for register addresses, argument 2 must be a register", .{ @tagName(instruction.operation)[2..], @tagName(instruction.suffix.?) });
                        try self.parseError(dummy_token, err_msg);
                        return null;
                    }

                    if (arg3 != null and arg3.?.type == .register) {
                        const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'{s}' argument 3 must be an immediate", .{@tagName(instruction.operation)[2..]});
                        try self.parseError(dummy_token, err_msg);
                        return null;
                    }

                    if (arg3 != null and (arg3.?.value > std.math.maxInt(i7) or arg3.?.value < std.math.minInt(i7))) {
                        const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'{s}' argument 3 does not fit in a 7-bit integer [{d},{d}]", .{ @tagName(instruction.operation)[2..], std.math.minInt(i7), std.math.maxInt(i7) });
                        try self.parseError(dummy_token, err_msg);
                        return null;
                    }

                    if (arg3 != null) {
                        e = @bitCast(@as(i8, @truncate(arg3.?.value << 1)));
                    } else {
                        e = switch (instruction.suffix.?) {
                            .ia, .ib => @as(u8, @bitCast(@as(i8, -1))),
                            .da, .db => @as(u8, 1),
                        } << 1;
                    }
                    e |= switch (instruction.suffix.?) {
                        .ia, .da => 0,
                        .ib, .db => 1,
                    };
                } else if (arg3 != null) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'{s}' takes 2 arguments when used without address register modification: xd xs/imm", .{@tagName(instruction.operation)[2..]});
                    try self.parseError(dummy_token, err_msg);
                    return null;
                }

                if (instruction.flag and instruction.operation == .i_str) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'{s}' cannot set flags", .{@tagName(instruction.operation)[2..]});
                    try self.parseError(dummy_token, err_msg);
                    return null;
                }

                return ParsedInstruction{
                    .instruction = instruction,
                    .destination = @truncate(@as(u32, @intCast(arg1.?.value & 0xF))),
                    .op1 = 0,
                    .op2 = arg2.?.value,
                    .immediate = (arg2.?.type != .register),
                    .e_bits = e,
                };
            },
            .i_ldh => {
                // ldh xd imm
                if (arg1 == null or arg2 == null) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'{s}' takes 2 arguments: xd imm", .{@tagName(instruction.operation)[2..]});
                    try self.parseError(dummy_token, err_msg);
                    return null;
                }

                if (arg1.?.type != .register) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'{s}' argument 1 must be a register", .{@tagName(instruction.operation)[2..]});
                    try self.parseError(dummy_token, err_msg);
                    return null;
                }

                if (arg2.?.type == .register) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'{s}' argument 2 must be an immediate", .{@tagName(instruction.operation)[2..]});
                    try self.parseError(dummy_token, err_msg);
                    return null;
                }

                return ParsedInstruction{ .instruction = instruction, .destination = @truncate(@as(u32, @intCast(arg1.?.value & 0xF))), .op1 = 0, .op2 = arg2.?.value & 0xFFFF, .immediate = true };
            },
            .i_b => {
                // b xs/imm
                if (arg1 == null or arg2 != null) {
                    const err_msg = try std.fmt.bufPrint(&self.err_buffer, "'{s}' takes 2: xs/imm", .{@tagName(instruction.operation)[2..]});
                    try self.parseError(dummy_token, err_msg);
                    return null;
                }

                return ParsedInstruction{ .instruction = instruction, .destination = 0, .op1 = 0, .op2 = arg1.?.value, .immediate = (arg1.?.type != .register) };
            },
        }
    }

    fn parseLabelFirst(self: *Parser) !void {
        const ident = self.lexer.lex();
        if (ident.type != .identifier) {
            const err_msg = try std.fmt.bufPrint(&self.err_buffer, "expected identifier, got '{s}'", .{if (ident.slice[0] != '\n') ident.slice else "\\n"});
            return self.parseError(ident, err_msg);
        }

        // Check if symbol can be defined
        if (self.symbols.contains(ident.slice)) {
            const err_msg = try std.fmt.bufPrint(&self.err_buffer, "name '{s}' already defined", .{ident.slice});
            return self.parseError(ident, err_msg);
        }

        // Value is the address after the current instruction
        try self.symbols.put(ident.slice, Symbol{ .type = .label, .value = @intCast(self.program.items.len) });
    }
};
