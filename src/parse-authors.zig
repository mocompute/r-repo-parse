const std = @import("std");
const cmdline = @import("cmdline");
const mos = @import("mos");
const mosql = @import("mosql");
const StringStorage = @import("string_storage.zig").StringStorage;
const Authors = @import("Authors.zig");

const Options = .{
    .{ "help", false },
    .{ "db", 0 }, // suppress -d option
    .{ "verbose", false },
};

const DB_FILE = "parse-authors.db";
var _verbose = false;

const usage =
    \\Usage: parse-authors [options] <packages-file>
    \\
    \\where <packages-file> may be plain text or gzipped file containing
    \\one or more Package stanzas with an Authors@R field.
    \\
    \\Options:
    \\  --db <file>    Change default SQLite3 db filename [parse-authors.db]
    \\  --help, -h     Display help
    \\  --verbose, -v  Enable verbose logging
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
    if (options.present("verbose")) {
        _verbose = true;
    }

    if (options.positional().len < 1) {
        std.debug.print("error: must provide at least one file argument\n", .{});
        std.debug.print("{s}", .{usage});
        std.process.exit(1);
    }

    const db_file = b: {
        if (options.get("db")) |x| break :b x;
        break :b DB_FILE;
    };

    log("parse-authors: using db file: {s}\n", .{db_file});

    // set up Authors
    var strings = try StringStorage.init(alloc, std.heap.page_allocator);
    defer strings.deinit();

    var authors = Authors.init(alloc);
    defer authors.deinit();

    var timer = try std.time.Timer.start();

    // read files
    for (options.positional()) |file| {
        try read_file(alloc, &authors, &strings, file);
        log("Parsing '{s}' took {}ms\n", .{ file, @divFloor(timer.lap(), 1_000_000) });
    }

    // summary stats
    if (_verbose)
        authors.db.debugPrintInfo();
}

fn read_file(alloc: std.mem.Allocator, authors: *Authors, strings: *StringStorage, path: []const u8) !void {
    std.fs.cwd().access(path, .{}) catch |err| {
        std.debug.print("error: could not access '{s}': {s}\n", .{ path, @errorName(err) });
        return;
    };

    log("reading file '{s}'\n", .{path});

    const source: ?[]const u8 = try mos.file.readFileMaybeGzip(alloc, path);
    try std.testing.expect(source != null);
    defer if (source) |s| alloc.free(s);

    if (source) |source_| {
        const parse_log = try authors.read(source_, strings);

        for (parse_log) |x| switch (x.tag) {
            .warn, .err => log("{}\n", .{x}),
            .info => {},
        };
    }
}

fn log(comptime fmt: []const u8, args: anytype) void {
    if (_verbose)
        std.debug.print(fmt, args);
}
