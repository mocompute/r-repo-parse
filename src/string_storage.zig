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

const std = @import("std");
const testing = std.testing;
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const StringHashMap = std.StringHashMap;

/// Arena storage for de-duplicated strings. Strings are copied to the
/// arena, and an index is maintained in a StringHashMap to avoid
/// multiple copies of the same string. Use `append` to add a string
/// or look up an existing string. A slice is returned.
pub const StringStorage = struct {
    alloc: Allocator,
    arena: ArenaAllocator,
    index: std.StringHashMap([]u8),

    /// Use alloc to expand the StringHashMap, and use child_allocator
    /// to expand the arena.
    pub fn init(alloc: Allocator, child_allocator: Allocator) !Self {
        return StringStorage.initCapacity(alloc, child_allocator, 0);
    }

    /// Use alloc to expand the StringHashMap, and use child_allocator
    /// to expand the arena. ArenaAllocator is pre-allocated with the
    /// requested capacity, and the StringHashMap is preheated.
    pub fn initCapacity(alloc: Allocator, child_allocator: Allocator, capacity: usize) !Self {
        var arena = ArenaAllocator.init(child_allocator);
        var index = StringHashMap([]u8).init(alloc); // not arena

        if (capacity != 0) {
            // reserve capacity/preheat
            _ = try arena.allocator().alloc(u8, capacity);
            if (!arena.reset(.retain_capacity)) return error.OutOfMemory;

            // StringHashMap has a reduced capacity and defines its own Size type (u32)
            if (capacity > std.math.maxInt(@TypeOf(index).Size)) return error.OutOfMemory;
            try index.ensureTotalCapacity(@intCast(capacity));
        }

        return .{
            .alloc = alloc,
            .arena = arena,
            .index = index,
        };
    }

    /// Release buffers and invalidate our struct.
    pub fn deinit(self: *Self) void {
        self.index.deinit();
        self.arena.deinit();
        self.* = undefined;
    }

    /// Reset arena and index, returing true if successful.
    pub fn reset(self: *Self, mode: ArenaAllocator.ResetMode) bool {
        if (self.arena.reset(mode)) {
            self.index.clearRetainingCapacity();
            return true;
        } else {
            return false;
        }
    }

    /// Return a slice pointing to our managed storage of string data,
    /// copying string to our arena if necessary.
    pub fn append(self: *Self, string: anytype) ![]const u8 {
        // if the string is in our index, return it
        if (self.index.get(string)) |v|
            return v;

        const alloc = self.arena.allocator();

        // dupe the string so the caller can release its memory
        const ours = try alloc.dupe(u8, string);

        try self.index.putNoClobber(ours, ours);

        return ours;
    }

    pub const Self = @This();
};

test "string_storage basic usage" {
    const alloc = std.testing.allocator;

    var ss = try StringStorage.init(alloc, std.heap.page_allocator);
    defer ss.deinit();
    const d1 = "hello world";
    const d2_ = "another string";

    var d2 = std.BoundedArray(u8, d2_.len){};
    try d2.appendSlice(d2_);
    d2.set(0, 'x');

    const s1 = try ss.append(d1);
    const s2 = try ss.append(d2.slice());

    const big = try alloc.alloc(u8, 1024 * 1024);
    defer alloc.free(big);

    const sbig = try ss.append(big);

    // pointers still valid
    try testing.expectEqualStrings("hello world", s1);
    try testing.expectEqualStrings("xnother string", s2);
    try testing.expectEqual(1024 * 1024, sbig.len);

    // testing index, same slice returned
    const s3 = try ss.append("hello world");
    try testing.expectEqual(s1, s3);
}
