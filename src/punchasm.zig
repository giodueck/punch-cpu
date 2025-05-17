const std = @import("std");
const clap = @import("clap");
const mvzr = @import("mvzr");

const parser = @import("parser.zig");

const stderr = std.io.getStdErr().writer();
const stdout = std.io.getStdOut().writer();

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    defer _ = gpa.detectLeaks();
    const alloc = gpa.allocator();

    // First we specify what parameters our program can take.
    const params = [_]clap.Param(u8){
        .{
            .id = 'h',
            .names = .{ .short = 'h', .long = "help" },
        },
        .{ .id = 'f', .takes_value = .one },
    };

    const usageStr =
        "Usage: [-h] <source file>\n";

    var iter = try std.process.ArgIterator.initWithAllocator(alloc);
    defer iter.deinit();

    // Skip exe argument.
    _ = iter.next();

    // Initialize our diagnostics, which can be used for reporting useful errors.
    // This is optional. You can also leave the `diagnostic` field unset if you
    // don't care about the extra information `Diagnostic` provides.
    var diag = clap.Diagnostic{};
    var cla_parser = clap.streaming.Clap(u8, std.process.ArgIterator){
        .params = &params,
        .iter = &iter,
        .diagnostic = &diag,
    };

    // Assembler variables
    var source_filename: ?[]const u8 = null;

    // Because we use a streaming parser, we have to consume each argument parsed individually.
    while (cla_parser.next() catch |err| {
        // Report useful error and exit.
        diag.report(std.io.getStdErr().writer(), err) catch {};
        try stderr.writeAll(usageStr);
        return 1;
    }) |arg| {
        // arg.param will point to the parameter which matched the argument.
        switch (arg.param.id) {
            // 'h' => std.debug.print("Help!\n", .{}),
            // 'n' => std.debug.print("--number = {s}\n", .{arg.value.?}),

            'h' => {
                try stderr.writeAll(usageStr);
                return 0;
            },

            // arg.value == null, if arg.param.takes_value == .none.
            // Otherwise, arg.value is the value passed with the argument, such as "-a=10"
            // or "-a 10".
            'f' => {
                if (source_filename != null) {
                    try stderr.print("Too many arguments\n", .{});
                    try stderr.writeAll(usageStr);
                    return 1;
                }
                source_filename = arg.value;
            },
            else => unreachable,
        }
    }

    if (source_filename == null) {
        try stderr.writeAll("Missing source file\n");
        try stderr.writeAll(usageStr);
        return 1;
    }

    const file = std.fs.cwd().openFile(source_filename.?, .{}) catch |e| {
        try stderr.print("Could not open file '{s}': {s}\n", .{ source_filename.?, @errorName(e) });
        return 1;
    };
    defer file.close();
    const raw_program = try file.readToEndAlloc(alloc, 0x40000);
    defer alloc.free(raw_program);

    // const word: mvzr.Regex = mvzr.compile("^\\w+").?;
    // const whitespace: mvzr.Regex = mvzr.compile("^[ \\t]+").?;
    // const nl: mvzr.Regex = mvzr.compile("^\n+").?;
    // const line: mvzr.Regex = mvzr.compile("^[^\n]*").?;

    // var cursor: usize = 0;
    // while (raw_program[cursor..].len != 0) {
    //     var whitespace_match = whitespace.match(raw_program[cursor..]);
    //     if (whitespace_match) |m| {
    //         cursor += (m.end - m.start);
    //     }
    //     const newline_match = nl.match(raw_program[cursor..]);
    //     if (newline_match) |m| {
    //         cursor += (m.end - m.start);
    //     }
    //     whitespace_match = whitespace.match(raw_program[cursor..]);
    //     if (whitespace_match) |m| {
    //         cursor += (m.end - m.start);
    //     }
    //
    //     if (raw_program[cursor..].len == 0)
    //         break;
    //
    //     const match: ?mvzr.Match = word.match(raw_program[cursor..]);
    //
    //     if (match) |m| {
    //         try stderr.print("{s}\n", .{m.slice});
    //         cursor += m.slice.len;
    //     } else {
    //         const rest_of_line = line.match(raw_program[cursor..]);
    //         try stderr.print("Error: Unmatched tokens: {s}\n", .{rest_of_line.?.slice});
    //         cursor += rest_of_line.?.slice.len;
    //     }
    // }

    var p = parser.Parser{ .input = raw_program };
    try p.init(alloc);
    defer p.deinit();

    const error_count = try p.parse();

    // if (error_count == 0) {
    //     // output_program_name: []const u8 = "out_program.txt",
    //     // output_data_name: []const u8 = "out_data.txt",
    //     const prog_bp = std.fs.cwd().createFile(p.output_program_name, .{}) catch |e| {
    //         try stderr.print("Could not create file '{s}': {s}\n", .{ p.output_program_name, @errorName(e) });
    //         return 1;
    //     };
    //     defer prog_bp.close();
    //
    //     const data_bp = std.fs.cwd().createFile(p.output_program_name, .{}) catch |e| {
    //         try stderr.print("Could not create file '{s}': {s}\n", .{ p.output_program_name, @errorName(e) });
    //         return 1;
    //     };
    //     defer data_bp.close();
    // }

    return if (error_count > 0) 1 else 0;
}
