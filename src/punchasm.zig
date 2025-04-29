const std = @import("std");
const clap = @import("clap");
const mvzr = @import("mvzr");

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
    var parser = clap.streaming.Clap(u8, std.process.ArgIterator){
        .params = &params,
        .iter = &iter,
        .diagnostic = &diag,
    };

    // Assembler variables
    var source_filename: ?[]const u8 = null;

    // Because we use a streaming parser, we have to consume each argument parsed individually.
    while (parser.next() catch |err| {
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
                std.debug.print("{c}: {s}\n", .{ arg.param.id, arg.value.? });
                source_filename = arg.value.?;
            },
            else => unreachable,
        }
    }

    const regex: mvzr.Regex = mvzr.compile("^[ab]c").?;
    const isMatch: bool = regex.isMatch(source_filename.?);
    const match: ?mvzr.Match = regex.match(source_filename.?);

    try stderr.print("{s} {s}\n", .{source_filename.?, if (isMatch) "matches" else "does not match" });
    if (isMatch)
        try stderr.print("{s}\n", .{match.?.slice});

    return 0;
}
