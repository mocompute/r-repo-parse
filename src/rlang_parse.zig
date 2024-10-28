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
const StringStorage = @import("string_storage.zig").StringStorage;

const Token = union(enum) {
    open_round,
    close_round,
    identifier: []const u8,
    string: []const u8,
    equal,
    comma,

    pub fn eq(self: Token, other: Token) bool {
        const Tag = std.meta.Tag(@TypeOf(self));
        if (@as(Tag, self) != @as(Tag, other)) return false;
        return switch (self) {
            .identifier => std.mem.eql(u8, self.identifier, other.identifier),
            .string => std.mem.eql(u8, self.string, other.string),
            else => true,
        };
    }
};

const TokenizeResult = union(enum) {
    ok: []Token,
};

/// Caller must free returned token slice with provided allocator.
pub fn tokenize(alloc: Allocator, source: []const u8, strings: *StringStorage) TokenizeResult {
    _ = alloc;
    _ = source;
    _ = strings;
    return .{ .ok = &.{} };
}

test "tokenize" {
    const alloc = std.testing.allocator;
    const source =
        \\c()
        \\
    ;

    var strings = try StringStorage.init(alloc, std.heap.page_allocator);
    defer strings.deinit();

    const res = tokenize(alloc, source, &strings);
    switch (res) {
        .ok => |toks| {
            defer alloc.free(toks);
            try expectTokens(&.{.{ .identifier = "c" }}, toks);
        },
    }
}

fn expectTokens(expect: []const Token, actual: []const Token) !void {
    if (expect.len != actual.len) return error.UnequalLengths;

    // langref: lengths must be equal otherwise detectable illegal
    // behaviour, so the above if test is not strictly needed for a
    // test function
    for (expect, actual) |e, a| {
        if (!e.eq(a)) return error.TokenizeError;
    }
}

const Allocator = std.mem.Allocator;
