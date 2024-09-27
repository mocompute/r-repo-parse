// This file is part of r-repo-parse.
//
// Copyright (C) 2024 <https://codeberg.org/mocompute>
//
// r-repo-parse is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// r-repo-parse is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

/// Represents a package repository and provides a parser to update
/// itself from a Debian Control File (DCF), as used in standard R
/// package repository PACKAGES files.
pub const Repository = struct {
    alloc: Allocator,
    strings: StringStorage,
    packages: std.MultiArrayList(Package),
    parse_error: ?Parser.ParseError = null,

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

    /// Return package(s) information for given NameAndVersionConstraint.
    pub fn findPackage(
        self: Repository,
        alloc: Allocator,
        navc: NameAndVersionConstraint,
        comptime options: struct { max_results: u32 = 16 },
    ) error{OutOfMemory}![]Package {
        var out = try std.ArrayList(Package).initCapacity(alloc, options.max_results);
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
    /// NameAndVersionConstraint. If there are multiple packages that
    /// satisfy the constraint, return the one with the highest
    /// version.
    pub fn findLatestPackage(
        self: Repository,
        alloc: Allocator,
        navc: NameAndVersionConstraint,
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

    /// Create an index of this repository. Caller must call deinit.
    pub fn createIndex(self: Repository) !Index {
        return Index.init(self);
    }

    /// Read packages information from provided source. Expects Debian
    /// Control File format, same as R PACKAGES file. Returns number
    /// of packages found.
    pub fn read(self: *Repository, name: []const u8, source: []const u8) !usize {
        var count: usize = 0;
        var parser = try parse.Parser.init(self.alloc, &self.strings);
        defer parser.deinit();
        parser.parse(source) catch |err| switch (err) {
            error.ParseError => |e| {
                self.parse_error = parser.parse_error;
                return e;
            },
            else => |e| {
                return e;
            },
        };

        // reserve estimated space and free before exit (empirical from CRAN PACKAGES)
        try self.packages.ensureTotalCapacity(self.alloc, parser.nodes.items.len / 30);
        defer self.packages.shrinkAndFree(self.alloc, self.packages.len);

        // reserve estimated additional space for strings
        // FIXME: preallocate string storage earlier
        // try self.strings.ensureCapacity(parser.nodes.items.len / 30 * 16);

        // reserve working list of []NameAndVersionConstraint
        var nav_list = try std.ArrayList(NameAndVersionConstraint).initCapacity(self.alloc, 16);
        defer nav_list.deinit();

        const empty_package: Package = .{ .repository = try self.strings.append(name) };
        var result = empty_package;

        const nodes = parser.nodes.items;
        var idx: usize = 0;
        var node: Parser.Node = undefined;
        while (true) : (idx += 1) {
            node = nodes[idx];

            switch (node) {
                .eof => break,

                .stanza_end => {
                    try self.packages.append(self.alloc, result);
                    result = empty_package;
                    nav_list.clearRetainingCapacity();
                    count += 1;
                },

                .field => |field| {
                    if (std.mem.eql(u8, "Package", field.name)) {
                        result.name = try parsePackageName(nodes, &idx, &self.strings);
                    } else if (std.mem.eql(u8, "Version", field.name)) {
                        result.version = try parsePackageVersion(nodes, &idx);
                        idx -= 1; // backtrack
                        result.version_string = try parsePackageVersionString(nodes, &idx, &self.strings);
                    } else if (std.mem.eql(u8, "Depends", field.name)) {
                        try parsePackages(nodes, &idx, &nav_list);
                        result.depends = try nav_list.toOwnedSlice();
                    } else if (std.mem.eql(u8, "Suggests", field.name)) {
                        try parsePackages(nodes, &idx, &nav_list);
                        result.suggests = try nav_list.toOwnedSlice();
                    } else if (std.mem.eql(u8, "Imports", field.name)) {
                        try parsePackages(nodes, &idx, &nav_list);
                        result.imports = try nav_list.toOwnedSlice();
                    } else if (std.mem.eql(u8, "LinkingTo", field.name)) {
                        try parsePackages(nodes, &idx, &nav_list);
                        result.linkingTo = try nav_list.toOwnedSlice();
                    }
                },

                else => continue,
            }
        }
        return count;
    }

    fn parsePackageName(nodes: []Parser.Node, idx: *usize, strings: *StringStorage) ![]const u8 {
        idx.* += 1;
        switch (nodes[idx.*]) {
            .name_and_version => |nv| {
                return try strings.append(nv.name);
            },
            // expect .name_and_version immediately after .field for a Package field
            else => unreachable,
        }
    }

    fn parsePackageVersion(nodes: []Parser.Node, idx: *usize) !Version {
        idx.* += 1;
        switch (nodes[idx.*]) {
            .string_node => |s| {
                return try Version.parse(s.value);
            },
            // expect .string_node immediately after .field for a Version field
            else => unreachable,
        }
    }

    fn parsePackageVersionString(nodes: []Parser.Node, idx: *usize, strings: *StringStorage) ![]const u8 {
        idx.* += 1;
        switch (nodes[idx.*]) {
            .string_node => |s| {
                return try strings.append(s.value);
            },
            // expect .string_node immediately after .field for a Version field
            else => unreachable,
        }
    }

    fn parsePackages(
        nodes: []Parser.Node,
        idx: *usize,
        list: *std.ArrayList(NameAndVersionConstraint),
    ) !void {
        idx.* += 1;
        while (true) : (idx.* += 1) {
            const node = nodes[idx.*];
            switch (node) {
                .name_and_version => |nv| {
                    try list.append(NameAndVersionConstraint{
                        .name = nv.name,
                        .version_constraint = nv.version_constraint,
                    });
                },
                else => break,
            }
        }
    }

    //
    // -- iterator -----------------------------------------------------------
    //

    /// Represents a single package and its dependencies.
    pub const Package = struct {
        name: []const u8 = "",
        version: Version = .{},
        version_string: []const u8 = "",
        repository: []const u8 = "",
        depends: []NameAndVersionConstraint = &.{},
        suggests: []NameAndVersionConstraint = &.{},
        imports: []NameAndVersionConstraint = &.{},
        linkingTo: []NameAndVersionConstraint = &.{},
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
        navc: NameAndVersionConstraint,
    ) error{ OutOfMemory, NotFound }![]NameAndVersionConstraint {
        var arena = std.heap.ArenaAllocator.init(alloc);
        defer arena.deinit();

        var out = NameAndVersionConstraintHashMap.init(alloc);
        defer out.deinit();

        if (try self.findLatestPackage(alloc, navc)) |root_package| {
            try self.doTransitiveDependencies(&arena, root_package, &out);
            return try alloc.dupe(NameAndVersionConstraint, out.keys());
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
        navc: NameAndVersionConstraint,
    ) error{ OutOfMemory, NotFound }![]NameAndVersionConstraint {
        var arena = std.heap.ArenaAllocator.init(alloc);
        defer arena.deinit();

        var out = NameAndVersionConstraintHashMap.init(alloc);
        defer out.deinit();

        if (try self.findLatestPackage(alloc, navc)) |root_package| {
            try self.doTransitiveDependencies(&arena, root_package, &out);

            var result = try std.ArrayList(NameAndVersionConstraint).initCapacity(alloc, out.count());
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
        out: *NameAndVersionConstraintHashMap,
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
};

//
// -- test -------------------------------------------------------------------
//

test "PACKAGES.gz" {
    const path = "PACKAGES.gz";
    std.fs.cwd().access(path, .{}) catch return;
    const alloc = testing.allocator;

    var source: ?[]const u8 = try mos.file.readFileMaybeGzip(alloc, path);
    try testing.expect(source != null);
    defer if (source) |s| alloc.free(s);

    var timer = try std.time.Timer.start();

    var strings = try StringStorage.init(alloc, std.heap.page_allocator);
    defer strings.deinit();
    var parser = try parse.Parser.init(alloc, &strings);
    defer parser.deinit();
    parser.parse(source.?) catch |err| switch (err) {
        error.ParseError => {
            if (parser.parse_error) |perr| {
                std.debug.print("ERROR: ParseError: {s}: {}:{s}\n", .{
                    perr.message,
                    perr.token,
                    source.?[perr.token.loc.start..perr.token.loc.end],
                });
            }
        },
        error.OutOfMemory => {
            std.debug.print("ERROR: OutOfMemory\n", .{});
        },
        else => unreachable,
    };

    std.debug.print("Parse to AST only = {}ms\n", .{@divFloor(timer.lap(), 1_000_000)});
    std.debug.print("Parser nodes: {d}\n", .{parser.nodes.items.len});
    std.debug.print("Number of stanzas parsed: {d}\n", .{parser.numStanzas()});

    // read entire repo
    var repo = try Repository.init(alloc);
    defer repo.deinit();
    _ = try repo.read("test repo", source.?);
    std.debug.print(
        "Parse to Repository ({} packages) = {}ms\n",
        .{ repo.packages.len, @divFloor(timer.lap(), 1_000_000) },
    );

    // after parser.parse() returns, we should be able to immediate
    // release the source. Note that repo.read() also uses it in this
    // test.
    if (source) |s| alloc.free(s);
    source = null;

    // Current PACKAGES has this as the first stanza:
    // Package: A3
    // Version: 1.0.0
    try testing.expectEqualStrings("A3", repo.packages.items(.name)[0]);
    try testing.expectEqual(1, repo.packages.items(.version)[0].major);
    try testing.expectEqualStrings("1.0.0", repo.packages.items(.version_string)[0]);

    try testing.expectEqualStrings("AalenJohansen", repo.packages.items(.name)[1]);
    try testing.expectEqual(1, repo.packages.items(.version)[1].major);
    try testing.expectEqualStrings("AATtools", repo.packages.items(.name)[2]);
    try testing.expectEqual(0, repo.packages.items(.version)[2].major);

    const pack = try repo.findLatestPackage(alloc, .{ .name = "A3" });
    try testing.expect(pack != null);
    try testing.expectEqualStrings("A3", pack.?.name);

    // index
    var index = try repo.createIndex();
    defer index.deinit();
    try testing.expect(index.items.count() <= repo.packages.len);

    std.debug.print("Index count = {}\n", .{index.items.count()});

    try testing.expectEqual(0, index.items.get("A3").?.single.index);
    try testing.expectEqual(1, index.items.get("A3").?.single.version.major);
    try testing.expectEqual(1, index.items.get("AalenJohansen").?.single.index);
    try testing.expectEqual(1, index.items.get("AalenJohansen").?.single.version.major);
    try testing.expectEqual(2, index.items.get("AATtools").?.single.index);
    try testing.expectEqual(0, index.items.get("AATtools").?.single.version.major);

    const jsonlite = try repo.findLatestPackage(alloc, .{ .name = "jsonlite" });
    try testing.expect(jsonlite != null);
}

test "PACKAGES sanity check" {
    const path = "PACKAGES.gz";
    std.fs.cwd().access(path, .{}) catch return;
    const alloc = testing.allocator;
    const source: ?[]const u8 = try mos.file.readFileMaybeGzip(alloc, path);
    errdefer if (source) |s| alloc.free(s);

    var repo = try Repository.init(alloc);
    defer repo.deinit();
    if (source) |s| _ = try repo.read("test", s);
    if (source) |s| alloc.free(s);

    var index = try repo.createIndex();
    defer index.deinit();

    var unsatisfied = std.StringHashMap(std.ArrayList(NameAndVersionConstraint)).init(alloc);
    defer {
        var it = unsatisfied.iterator();
        while (it.next()) |x| x.value_ptr.deinit();
        unsatisfied.deinit();
    }

    var it = repo.iter();
    while (it.next()) |p| {
        const deps = try Repository.Tools.unsatisfied(index, alloc, p.depends);
        const impo = try Repository.Tools.unsatisfied(index, alloc, p.imports);
        const link = try Repository.Tools.unsatisfied(index, alloc, p.linkingTo);
        defer alloc.free(deps);
        defer alloc.free(impo);
        defer alloc.free(link);

        const res = try unsatisfied.getOrPut(p.name);
        if (!res.found_existing) res.value_ptr.* = std.ArrayList(NameAndVersionConstraint).init(alloc);
        try res.value_ptr.appendSlice(deps);
        try res.value_ptr.appendSlice(impo);
        try res.value_ptr.appendSlice(link);
    }

    var un_it = unsatisfied.iterator();
    while (un_it.next()) |u| {
        for (u.value_ptr.items) |nav| {
            std.debug.print("Package '{s}' dependency '{s}' version '{s}' not satisfied.\n", .{
                u.key_ptr.*,
                nav.name,
                nav.version_constraint,
            });
        }
    }
}

test "find latest package" {
    const alloc = testing.allocator;
    const data1 =
        \\Package: foo
        \\Version: 1.0
        \\
        \\Package: foo
        \\Version: 1.0.1
    ;
    const data2 =
        \\Package: foo
        \\Version: 1.0.2
        \\
        \\Package: foo
        \\Version: 1.0.1
    ;

    {
        var repo = try Repository.init(alloc);
        defer repo.deinit();
        _ = try repo.read("test", data1);

        const package = try repo.findLatestPackage(alloc, .{ .name = "foo" });
        try testing.expectEqualStrings("foo", package.?.name);
        try testing.expectEqual(Version{ .major = 1, .minor = 0, .patch = 1, .rev = 0 }, package.?.version);
    }
    {
        var repo = try Repository.init(alloc);
        defer repo.deinit();
        _ = try repo.read("test", data2);

        const package = try repo.findLatestPackage(alloc, .{ .name = "foo" });
        try testing.expectEqualStrings("foo", package.?.name);
        try testing.expectEqualStrings("test", package.?.repository);
        try testing.expectEqual(Version{ .major = 1, .minor = 0, .patch = 2, .rev = 0 }, package.?.version);
    }

    {
        var repo = try Repository.init(alloc);
        defer repo.deinit();
        _ = try repo.read("test", data2);
        var index = try repo.createIndex();
        defer index.deinit();

        const package_index = index.findPackage(NameAndVersionConstraint{ .name = "foo", .version_constraint = .{
            .operator = .gt,
            .version = .{
                .major = 1,
                .patch = 1,
            },
        } });
        try testing.expectEqual(0, package_index.?);

        try testing.expectEqual(null, index.findPackage(NameAndVersionConstraint{ .name = "foo", .version_constraint = .{
            .operator = .gt,
            .version = .{
                .major = 1,
                .patch = 2,
            },
        } }));
    }
}

test "transitive dependencies" {
    const alloc = testing.allocator;
    const data1 =
        \\Package: parent
        \\Version: 1.0
        \\
        \\Package: child
        \\Version: 1.0
        \\Depends: parent (>= 1.0)
        \\
        \\Package: grandchild
        \\Version: 1.0
        \\Depends: child (>= 1.0)
    ;

    {
        var repo = try Repository.init(alloc);
        defer repo.deinit();
        _ = try repo.read("test", data1);

        const res = try repo.transitiveDependencies(alloc, .{ .name = "grandchild" });
        defer alloc.free(res);

        try testing.expectEqualDeep(
            res[0],
            NameAndVersionConstraint{ .name = "child", .version_constraint = try version.VersionConstraint.parse(.gte, "1.0") },
        );
    }
    {
        var repo = try Repository.init(alloc);
        defer repo.deinit();
        _ = try repo.read("test", data1);

        const res = try repo.transitiveDependenciesNoBase(alloc, .{ .name = "grandchild" });
        defer alloc.free(res);

        try testing.expectEqualDeep(
            res[0],
            NameAndVersionConstraint{ .name = "child", .version_constraint = try version.VersionConstraint.parse(.gte, "1.0") },
        );
    }
}

test "versions with minus" {
    const alloc = std.testing.allocator;
    const data =
        \\Package: whomadethis
        \\Version: 2.3-0
        \\Depends: base64enc (>= 0.1-3), rjson, parallel, R (>= 3.1.0)
        \\Imports: uuid, RCurl, unixtools, Rserve (>= 1.8-5), rediscc (>= 0.1-3), jsonlite, knitr, markdown, png, Cairo, httr, gist, mime, sendmailR, PKI
        \\Suggests: FastRWeb, RSclient, rcloud.client, rcloud.solr, rcloud.r
        \\License: MIT
        \\
    ;
    {
        var tokenizer = parse.Tokenizer.init(data);
        defer tokenizer.deinit();

        while (true) {
            const tok = tokenizer.next();
            if (tok.tag == .eof) break;
            tok.debugPrint(data);
        }
    }
    {
        var strings = try StringStorage.init(alloc, std.heap.page_allocator);
        defer strings.deinit();
        var parser = try parse.Parser.init(alloc, &strings);
        defer parser.deinit();
        parser.parse(data) catch |err| switch (err) {
            error.ParseError => |e| {
                // self.parse_error = parser.parse_error;
                return e;
            },
            else => |e| {
                return e;
            },
        };
        for (parser.nodes.items) |node| {
            std.debug.print("{}\n", .{node});
        }
    }

    {
        var repo = try Repository.init(alloc);
        defer repo.deinit();
        _ = try repo.read("test", data);

        std.debug.print("whomadethis:\n", .{});
        if (try repo.findLatestPackage(alloc, .{ .name = "whomadethis" })) |p| {
            std.debug.print("  Depends:\n", .{});
            for (p.depends) |x| {
                std.debug.print("    {s}\n", .{x.name});
            }
            std.debug.print("  Imports:\n", .{});
            for (p.imports) |x| {
                std.debug.print("    {s}\n", .{x.name});
            }
            std.debug.print("  LinkingTo:\n", .{});
            for (p.linkingTo) |x| {
                std.debug.print("    {s}\n", .{x.name});
            }
        }
    }
}

const std = @import("std");
const mem = std.mem;
const mos = @import("mos");
const testing = std.testing;
const Allocator = std.mem.Allocator;

const common = @import("common");
const StringStorage = common.StringStorage;

const parse = @import("parse.zig");
const Parser = parse.Parser;

pub const version = @import("version.zig");
const NameAndVersionConstraint = version.NameAndVersionConstraint;
const Version = version.Version;
const NameAndVersionConstraintHashMap = version.NameAndVersionConstraintHashMap;
