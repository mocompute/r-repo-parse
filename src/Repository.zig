alloc: Allocator,
strings: StringStorage,
packages: std.MultiArrayList(Package),
parse_error: ?ParseError = null,
stanza_error: ?dcf.StanzaParser.ErrorInfo = null,
field_error: ?dcf.FieldParser.ErrorInfo = null,

pub const Index = @import("Repository/Index.zig");
pub const Tools = @import("Repository/Tools.zig");

/// Caller must call deinit to release internal buffers.
pub fn init(alloc: Allocator) !Repository {
    return .{
        .alloc = alloc,
        .strings = try StringStorage.init(alloc, std.heap.page_allocator),
        .packages = .{},
    };
}

/// Release internal buffers and invalidate.
pub fn deinit(self: *Repository) void {
    const slice = self.packages.slice();
    for (slice.items(.depends)) |x| {
        self.alloc.free(x);
    }
    for (slice.items(.suggests)) |x| {
        self.alloc.free(x);
    }
    for (slice.items(.imports)) |x| {
        self.alloc.free(x);
    }
    for (slice.items(.linkingTo)) |x| {
        self.alloc.free(x);
    }
    self.strings.deinit();
    self.packages.deinit(self.alloc);
    self.* = undefined;
}

/// Return an iterator over the package data.
pub fn iter(self: Repository) Iterator {
    return Iterator.init(self);
}

/// Return the first package.
pub fn first(self: Repository) ?Package {
    var it = self.iter();
    return it.next();
}

/// Return one or more packages information for given rlang.PackageSpec.
pub fn findPackage(
    self: Repository,
    alloc: Allocator,
    navc: rlang.PackageSpec,
    comptime options: struct { max_results: u32 = 16 },
) error{OutOfMemory}![]Package {
    var out = try std.ArrayList(Package).initCapacity(alloc, options.max_results);
    defer out.deinit();
    const slice = self.packages.slice();
    var index: usize = 0;
    for (slice.items(.name)) |n| {
        if (mem.eql(u8, n, navc.name)) {
            if (navc.version_constraint.satisfied(slice.items(.version)[index])) {
                out.appendAssumeCapacity(slice.get(index));
                if (out.items.len == options.max_results) return error.OutOfMemory;
            }
        }
        index += 1;
    }
    return out.toOwnedSlice();
}

/// Return the latest package, if any, that satisfies the given
/// rlang.PackageSpec. If there are multiple packages that
/// satisfy the constraint, return the one with the highest
/// version.
pub fn findLatestPackage(
    self: Repository,
    alloc: Allocator,
    navc: rlang.PackageSpec,
) error{OutOfMemory}!?Package {
    const packages = try self.findPackage(alloc, navc, .{});
    defer alloc.free(packages);
    switch (packages.len) {
        0 => return null,
        1 => return packages[0],
        else => {
            var latest = packages[0];
            for (packages) |p| {
                if (p.version.order(latest.version) == .gt) latest = p;
            }
            return latest;
        },
    }
}

/// Read packages information from provided source. Source lifetime
/// must exceed Repository lifetime. Expects Debian Control File
/// format, same as R PACKAGES file. Returns number of packages found.
pub fn read(self: *Repository, name: []const u8, source: []const u8) (error{
    OutOfMemory,
    InvalidFormat,
    ParseError,
} || dcf.StanzaParser.Error || dcf.FieldParser.Error)!usize {
    const eql = std.ascii.eqlIgnoreCase;

    var count: usize = 0;

    // allocate a buffer large enough for maximum size of field
    // supported
    const buf = try self.alloc.alloc(u8, 16 * 1024);
    defer self.alloc.free(buf);

    // reserve estimated space required for packages and free before exit
    try self.packages.ensureTotalCapacity(self.alloc, source.len / 1024);
    defer self.packages.shrinkAndFree(self.alloc, self.packages.len);

    // parsers
    var stanzas = dcf.StanzaParser.init(source);
    var stanza_error = dcf.StanzaParser.ErrorInfo.empty;
    var fields = dcf.FieldParser.init("", buf);
    var field_error = dcf.FieldParser.ErrorInfo.empty;

    // state
    const empty_package: Package = .{ .repository = try self.strings.append(name) };
    var package = empty_package;
    errdefer package.deinit(self.alloc);

    while (true) {
        const stanza = stanzas.next(&stanza_error) catch |err| switch (err) {
            error.Eof => break,
            else => |e| {
                self.stanza_error = stanza_error;
                return e;
            },
        };

        fields.reset(stanza);

        while (true) {
            const field = fields.next(&field_error) catch |err| switch (err) {
                error.Eof => break,
                else => |e| {
                    self.field_error = field_error;
                    return e;
                },
            };

            if (eql("package", field.name)) {
                const owned = try self.strings.append(field.value);
                package.name = owned;
            } else if (eql("version", field.name)) {
                const owned = try self.strings.append(field.value);
                package.version_string = try self.strings.append(owned);
                package.version = try rlang.Version.parse(package.version_string);
            } else if (eql("depends", field.name)) {
                if (package.depends.len != 0) {
                    return self.parseError(package.name);
                }
                const owned = try self.strings.append(field.value);
                package.depends = try rlang.PackageSpec.parseListWithCapacity(self.alloc, owned, 16);
            } else if (eql("suggests", field.name)) {
                if (package.suggests.len != 0) {
                    return self.parseError(package.name);
                }
                const owned = try self.strings.append(field.value);
                package.suggests = try rlang.PackageSpec.parseListWithCapacity(self.alloc, owned, 16);
            } else if (eql("imports", field.name)) {
                if (package.imports.len != 0) {
                    return self.parseError(package.name);
                }
                const owned = try self.strings.append(field.value);
                package.imports = try rlang.PackageSpec.parseListWithCapacity(self.alloc, owned, 16);
            } else if (eql("linkingto", field.name)) {
                if (package.linkingTo.len != 0) {
                    return self.parseError(package.name);
                }
                const owned = try self.strings.append(field.value);
                package.linkingTo = try rlang.PackageSpec.parseListWithCapacity(self.alloc, owned, 16);
            }
        }

        if (package.name.len != 0) {
            try self.packages.append(self.alloc, package);
            package = empty_package;
            count += 1;
        }
    }
    return count;
}

fn parseError(self: *Repository, message: []const u8) error{ParseError} {
    self.parse_error = .{
        .message = message,
        .line = 0,
    };
    return error.ParseError;
}

const ParseError = struct {
    message: []const u8 = "",
    line: usize = 0,
};

//
// -- iterator -----------------------------------------------------------
//

/// Represents a single package and its dependencies.
pub const Package = struct {
    name: []const u8 = "",
    version: rlang.Version = .{},
    version_string: []const u8 = "",
    repository: []const u8 = "",
    depends: []rlang.PackageSpec = &.{},
    suggests: []rlang.PackageSpec = &.{},
    imports: []rlang.PackageSpec = &.{},
    linkingTo: []rlang.PackageSpec = &.{},

    /// Deinit a package that was allocated. May be called multiple
    /// times.
    pub fn deinit(self: *Package, alloc: Allocator) void {
        alloc.free(self.depends);
        alloc.free(self.suggests);
        alloc.free(self.imports);
        alloc.free(self.linkingTo);
        self.* = undefined;
    }
};

/// An iterator over a Repository.
pub const Iterator = struct {
    index: usize = 0,
    slice: std.MultiArrayList(Package).Slice,

    /// Return an iterator which provides one package at a time
    /// from the Repository.
    pub fn init(repo: Repository) Iterator {
        return .{
            .slice = repo.packages.slice(),
        };
    }

    pub fn next(self: *Iterator) ?Package {
        if (self.index < self.slice.len) {
            const out = self.index;
            self.index += 1;
            return self.slice.get(out);
        }
        return null;
    }
};

//
// -- transitive dependencies---------------------------------------------
//

/// Given a package name, return a slice of its transitive
/// dependencies. If there is more than one package with the same
/// name, select the latest version as the root. Caller must free
/// returned slice.
pub fn transitiveDependencies(
    self: Repository,
    alloc: Allocator,
    navc: rlang.PackageSpec,
) error{ OutOfMemory, NotFound }![]rlang.PackageSpec {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();

    var out = rlang.PackageSpecHashMap.init(alloc);
    defer out.deinit();

    if (try self.findLatestPackage(alloc, navc)) |root_package| {
        try self.doTransitiveDependencies(&arena, root_package, &out);
        return try alloc.dupe(rlang.PackageSpec, out.keys());
    } else return error.NotFound;
}

/// Given a package name, return a slice of its transitive
/// dependencies. If there is more than one package with the same
/// name, select the latest version as the root. Does not report
/// dependencies on base or recommended packages. Caller must free
/// returned slice.
pub fn transitiveDependenciesNoBase(
    self: Repository,
    alloc: Allocator,
    navc: rlang.PackageSpec,
) error{ OutOfMemory, NotFound }![]rlang.PackageSpec {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();

    var out = rlang.PackageSpecHashMap.init(alloc);
    defer out.deinit();

    if (try self.findLatestPackage(alloc, navc)) |root_package| {
        try self.doTransitiveDependencies(&arena, root_package, &out);

        var result = try std.ArrayList(rlang.PackageSpec).initCapacity(alloc, out.count());
        for (out.keys()) |x| {
            if (Tools.isBasePackage(x.name)) continue;
            if (Tools.isRecommendedPackage(x.name)) continue;
            result.appendAssumeCapacity(x);
        }
        return result.toOwnedSlice();
    } else return error.NotFound;
}

fn doTransitiveDependencies(
    self: Repository,
    arena: *std.heap.ArenaAllocator,
    package: Package,
    out: *rlang.PackageSpecHashMap,
) !void {
    for (package.depends) |navc| {
        if (Tools.isBasePackage(navc.name)) continue;
        if (Tools.isRecommendedPackage(navc.name)) continue;
        if (try self.findLatestPackage(arena.allocator(), navc)) |p| {
            try out.put(navc, true);
            try self.doTransitiveDependencies(arena, p, out);
        } else {
            std.debug.print("package {s} dependency not found: {}\n", .{ package.name, navc });
            return error.NotFound;
        }
    }
    for (package.imports) |navc| {
        if (Tools.isBasePackage(navc.name)) continue;
        if (Tools.isRecommendedPackage(navc.name)) continue;
        if (try self.findLatestPackage(arena.allocator(), navc)) |p| {
            try out.put(navc, true);
            try self.doTransitiveDependencies(arena, p, out);
        } else {
            std.debug.print("package {s} dependency not found: {}\n", .{ package.name, navc });
            return error.NotFound;
        }
    }
    for (package.linkingTo) |navc| {
        if (Tools.isBasePackage(navc.name)) continue;
        if (Tools.isRecommendedPackage(navc.name)) continue;
        if (try self.findLatestPackage(arena.allocator(), navc)) |p| {
            try out.put(navc, true);
            try self.doTransitiveDependencies(arena, p, out);
        } else {
            std.debug.print("package {s} dependency not found: {}\n", .{ package.name, navc });
            return error.NotFound;
        }
    }
}

/// Caller must free returned slice.
pub fn calculateInstallationOrder(
    self: Repository,
    packages: []Package,
    comptime options: struct {
        max_iterations: usize = 256,
    },
) ![]Package {
    var out = try std.ArrayList(Package).initCapacity(self.alloc, packages.len);
    out.appendSliceAssumeCapacity(packages);

    // earliest position a package is referenced
    var seen = std.StringArrayHashMap(usize).init(self.alloc);

    // first pass move all packages with zero deps to the front
    var pos: usize = 0;
    while (pos < out.items.len) : (pos += 1) {
        const p = out.items[pos];
        if (p.depends.len == 0 and p.imports.len == 0 and p.linkingTo.len == 0) {
            // std.debug.print("moving {s} to the front as it has no dependencies\n", .{p.name});
            out.insertAssumeCapacity(0, out.orderedRemove(pos));
        }
    }

    // shuffle packages when we find their current position is
    // after their earliest seen position.
    var iterations: usize = 0;
    while (iterations < options.max_iterations) : (iterations += 1) {
        var shuffled = false;

        // for each dependency, record the earliest position it is
        // seen. Needs to be done after each reshuffle.
        seen.clearRetainingCapacity();
        try recordEarliestDependents(out, &seen);

        pos = 0;
        while (pos < out.items.len) : (pos += 1) {
            const p = out.items[pos];

            if (seen.get(p.name)) |idx| {
                if (idx < pos) {
                    shuffled = true;
                    // std.debug.print("shuffling {s} from {} to {}\n", .{ p.name, pos, idx });

                    // do the remove/insert
                    std.debug.assert(idx < pos);
                    out.insertAssumeCapacity(idx, out.orderedRemove(pos));
                    try seen.put(p.name, idx);
                }
            }
        }

        if (!shuffled) break;
    }
    std.debug.print("returning after {} iterations.\n", .{iterations});
    return out.toOwnedSlice();
}

/// Caller owns the returned slice.
pub fn calculateInstallationOrderAll(self: Repository) ![]Package {
    var packages = try std.ArrayList(Package).initCapacity(self.alloc, self.packages.len);
    defer packages.deinit();

    var slice = self.packages.slice();
    defer slice.deinit(self.alloc);

    var index: usize = 0;
    while (index < slice.len) : (index += 1) {
        packages.appendAssumeCapacity(slice.get(index));
    }
    return self.calculateInstallationOrder(packages.items, .{});
}

fn recordEarliestDependents(packages: std.ArrayList(Package), seen: *std.StringArrayHashMap(usize)) !void {
    var pos: usize = 0;
    while (pos < packages.items.len) : (pos += 1) {
        const p = packages.items[pos];
        try recordEarliestDependentsOne(p, pos, seen);
    }
}

fn recordEarliestDependentsOne(p: Package, pos: usize, seen: *std.StringArrayHashMap(usize)) !void {
    for (p.depends) |x| {
        if (Tools.isBasePackage(x.name)) continue;
        if (Tools.isRecommendedPackage(x.name)) continue;
        // std.debug.print("{s} seen at {} by {s}\n", .{ x.name, pos, p.name });
        const gop = try seen.getOrPut(x.name);
        if (!gop.found_existing or gop.value_ptr.* > pos)
            gop.value_ptr.* = pos;
    }
    for (p.imports) |x| {
        if (Tools.isBasePackage(x.name)) continue;
        if (Tools.isRecommendedPackage(x.name)) continue;
        // std.debug.print("{s} seen at {} by {s}\n", .{ x.name, pos, p.name });
        const gop = try seen.getOrPut(x.name);
        if (!gop.found_existing or gop.value_ptr.* > pos)
            gop.value_ptr.* = pos;
    }
    for (p.linkingTo) |x| {
        if (Tools.isBasePackage(x.name)) continue;
        if (Tools.isRecommendedPackage(x.name)) continue;
        // std.debug.print("{s} seen at {} by {s}\n", .{ x.name, pos, p.name });
        const gop = try seen.getOrPut(x.name);
        if (!gop.found_existing or gop.value_ptr.* > pos)
            gop.value_ptr.* = pos;
    }
}

const Repository = @This();

const std = @import("std");
const mem = std.mem;
const mos = @import("mos");
const dcf = @import("dcf");
const rlang = @import("rlang");
const testing = std.testing;
const Allocator = std.mem.Allocator;

const StringStorage = @import("string_storage.zig").StringStorage;
