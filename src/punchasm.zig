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
        .{
            .id = 'p',
            .takes_value = .one,
            .names = .{ .short = 'p' },
        },
        .{
            .id = 'd',
            .takes_value = .one,
            .names = .{ .short = 'd' },
        },
        .{ .id = 'f', .takes_value = .one },
    };

    const usage_str =
        "Usage: punchasm [-h|--help]\n" ++
        "       punchasm [-p <program output file>][-d <data output file>] <source file>\n";

    const help_str = usage_str ++ "\n" ++
        "Options:\n" ++
        "   -h,--help   Print this help menu and exit\n\n" ++
        "   -p <str>    Program ROM output file. If both -p and -d are omitted, output is printed to stdout.\n" ++
        "               If -d is specified and -p is omitted, it defaults to 'prom.txt'\n\n" ++
        "   -d <str>    Data ROM output file. If both -p and -d are omitted, output is printed to stdout.\n" ++
        "               If -p is specified and -d is omitted, it defaults to 'drom.txt'\n" ++
        "               If there is no data declarations, the data ROM is omitted from output alltogether,\n" ++
        "               regardless of the usage of -d\n";

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
    var program_output: ?[]const u8 = null;
    var data_output: ?[]const u8 = null;

    // Because we use a streaming parser, we have to consume each argument parsed individually.
    while (cla_parser.next() catch |err| {
        // Report useful error and exit.
        diag.report(std.io.getStdErr().writer(), err) catch {};
        try stderr.writeAll(usage_str);
        return 1;
    }) |arg| {
        // arg.param will point to the parameter which matched the argument.
        switch (arg.param.id) {
            // 'h' => std.debug.print("Help!\n", .{}),
            // 'n' => std.debug.print("--number = {s}\n", .{arg.value.?}),

            'h' => {
                try stderr.writeAll(help_str);
                return 0;
            },

            // arg.value == null, if arg.param.takes_value == .none.
            // Otherwise, arg.value is the value passed with the argument, such as "-a=10"
            // or "-a 10".
            'f' => {
                if (source_filename != null) {
                    try stderr.print("Too many arguments\n", .{});
                    try stderr.writeAll(usage_str);
                    return 1;
                }
                source_filename = arg.value;
            },
            'p' => {
                program_output = arg.value;
            },
            'd' => {
                program_output = arg.value;
            },
            else => unreachable,
        }
    }

    if (source_filename == null) {
        try stderr.writeAll("Missing source file\n");
        try stderr.writeAll(usage_str);
        return 1;
    }

    if (program_output != null and data_output == null or program_output == null and data_output != null) {
        if (program_output == null) program_output = "prom.txt";
        if (data_output == null) data_output = "drom.txt";
    }

    const file = std.fs.cwd().openFile(source_filename.?, .{}) catch |e| {
        try stderr.print("Could not open file '{s}': {s}\n", .{ source_filename.?, @errorName(e) });
        return 1;
    };
    defer file.close();
    const raw_program = blk: {
        // Case insensitive
        var content = try file.readToEndAlloc(alloc, 0x40000);
        for (0..content.len) |i| {
            content[i] = std.ascii.toLower(content[i]);
        }
        break :blk content;
    };
    defer alloc.free(raw_program);

    var p = parser.Parser{ .input = raw_program };
    try p.init(alloc);
    defer p.deinit();

    const error_count = try p.parse();

    if (error_count == 0) {
        if (program_output) |filename| {
            const prog_bp = std.fs.cwd().createFile(filename, .{}) catch |e| {
                try stderr.print("Could not create file '{s}': {s}\n", .{ filename, @errorName(e) });
                return 1;
            };
            defer prog_bp.close();

            prog_bp.writer().print("{s}", .{p.output_program_bp.?}) catch |e| {
                try stderr.print("Could not write to file '{s}': {s}\n", .{ filename, @errorName(e) });
                return 1;
            };
        } else {
            try stdout.print("Program:  {s}\n\n", .{p.output_program_bp.?});
        }

        // If there is no data sections in the program, skip them in the output
        if (p.output_data_bp != null) {
            if (data_output) |filename| {
                const data_bp = std.fs.cwd().createFile(filename, .{}) catch |e| {
                    try stderr.print("Could not create file '{s}': {s}\n", .{ filename, @errorName(e) });
                    return 1;
                };
                defer data_bp.close();

                data_bp.writer().print("{s}", .{p.output_data_bp.?}) catch |e| {
                    try stderr.print("Could not write to file '{s}': {s}\n", .{ filename, @errorName(e) });
                    return 1;
                };
            } else {
                try stdout.print("Data:     {s}\n\n", .{p.output_data_bp.?});
            }
        }
    }

    return if (error_count > 0) 1 else 0;
}
