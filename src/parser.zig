const std = @import("std");

const lexer = @import("lexer.zig");
const Lexer = lexer.Lexer;
const Token = lexer.Token;
const Directive = lexer.Directive;

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

    // TODO separate into stages, as in notes.md
    pub fn parse(self: *Parser) !void {
        var token = self.lexer.lex();
        while (token.type != .eof) {
            std.debug.print("{any}: {s}\n", .{ token.type, token.slice });
            if (token.type == .instruction) std.debug.print("{any} \n", .{token.instruction});
            token = self.lexer.lex();
        }
    }

    fn parseError(self: *Parser, last: Token) void {
        var t = last;
        while (t.type != .newline) {
            t = self.lexer.lex();
        }
    }

    fn parseDirective(self: *Parser, directive: Directive) void {
        switch (directive) {
            // Constant format: "@const" identifier literal newline
            .constant => {
                // TODO print error messages
                const ident = self.lexer.lex();
                if (ident.type != .identifier) {
                    self.parseError(ident);
                    return;
                }

                const literal = self.lexer.lex();
                if (literal.type != .literal) {
                    self.parseError(literal);
                    return;
                }

                const newline = self.lexer.lex();
                if (newline.type != .literal) {
                    self.parseError(newline);
                    return;
                }
                // TODO do something with the correct input
            },
        }
    }
};
