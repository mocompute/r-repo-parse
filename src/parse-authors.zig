const std = @import("std");
const cmdline = @import("cmdline");

const Options = .{
    .{ "help", false },
};

const usage =
    \\Usage: parse-authors --help | -h
    \\       parse-authors PACKAGES
    \\       parse-authors PACKAGES.gz
    \\
;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var options = try cmdline.Options(.{}).init(alloc, Options);
    defer options.deinit();
    switch (options.parse()) {
        .err => |e| {
            std.debug.print("{!s}\n", .{e.toMessage(alloc)});
            std.debug.print("{s}", .{usage});
            std.process.exit(1);
        },
        else => {},
    }
    if (options.present("help")) {
        std.debug.print("{s}", .{usage});
        std.process.exit(0);
    }

    if (options.positional().len < 1) {
        std.debug.print("must provide at least one file argument\n", .{});
        std.debug.print("{s}", .{usage});
        std.process.exit(1);
    }

    std.debug.print("hello, world.\n", .{});
}
