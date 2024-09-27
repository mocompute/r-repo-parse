//! Represents an Index of a Repository.

const MapType = std.StringHashMap(AvailableVersions);

items: MapType,

const AvailableVersions = union(enum) {
    single: VersionIndex,
    multiple: std.ArrayList(VersionIndex),

    pub fn format(
        self: AvailableVersions,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;
        switch (self) {
            .single => |vi| {
                try writer.print("(IndexVersion.single {s} {})", .{
                    vi.version.string,
                    vi.index,
                });
            },
            .multiple => |l| {
                try writer.print("(IndexVersion.multiple", .{});
                for (l.items) |x| {
                    try writer.print(" {s}", .{x.version});
                }
                try writer.print(")", .{});
            },
        }
    }
};

const VersionIndex = struct { version: Version, index: usize };

/// Create an index of the repo. Uses the repository's
/// allocator for its internal buffers. Caller must deinit to
/// release buffers.
pub fn init(repo: Repository) !Index {
    // Index only supports up to max Index.Size items.
    if (repo.packages.len > std.math.maxInt(MapType.Size)) return error.OutOfMemory;
    var out = MapType.init(repo.alloc);
    try out.ensureTotalCapacity(@intCast(repo.packages.len));

    const slice = repo.packages.slice();
    const names = slice.items(.name);
    const versions = slice.items(.version);

    var idx: usize = 0;
    while (idx < repo.packages.len) : (idx += 1) {
        const name = names[idx];
        const ver = versions[idx];

        if (out.getPtr(name)) |p| {
            switch (p.*) {
                .single => |vi| {
                    p.* = .{
                        .multiple = std.ArrayList(VersionIndex).init(repo.alloc),
                    };
                    try p.multiple.append(vi);
                    try p.multiple.append(.{
                        .version = ver,
                        .index = idx,
                    });
                },
                .multiple => |*l| {
                    try l.append(.{
                        .version = ver,
                        .index = idx,
                    });
                },
            }
        } else {
            out.putAssumeCapacityNoClobber(name, .{
                .single = .{
                    .version = ver,
                    .index = idx,
                },
            });
        }
    }
    return .{ .items = out };
}

/// Release buffers and invalidate.
pub fn deinit(self: *Index) void {
    var it = self.items.valueIterator();
    while (it.next()) |v| switch (v.*) {
        .single => continue,
        .multiple => |l| {
            l.deinit();
        },
    };

    self.items.deinit();
    self.* = undefined;
}

/// Return index into repository packages for a package which
/// matches the requested constraint, or null.
pub fn findPackage(self: Index, package: NameAndVersionConstraint) ?usize {
    return if (self.items.get(package.name)) |entry| switch (entry) {
        .single => |e| if (package.version_constraint.satisfied(e.version)) e.index else null,
        .multiple => |es| b: {
            for (es.items) |e| {
                if (package.version_constraint.satisfied(e.version)) break :b e.index;
            }
            break :b null;
        },
    } else null;
}

const Index = @This();
const std = @import("std");
const Allocator = std.mem.Allocator;
const version = @import("../version.zig");
const NameAndVersionConstraint = version.NameAndVersionConstraint;
const Version = version.Version;
const NameAndVersionConstraintHashMap = version.NameAndVersionConstraintHashMap;

const repository = @import("../repository.zig");
const Repository = repository.Repository;
const isBasePackage = repository.isBasePackage;
const isRecommendedPackage = repository.isRecommendedPackage;
