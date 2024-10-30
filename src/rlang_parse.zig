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

// -- tokenizer ----------------------------------------------------

const Token = union(enum) {
    open_round,
    close_round,
    identifier: []const u8,
    string: []const u8,
    equal,
    comma,
    eof,

    pub fn eq(self: Token, other: Token) bool {
        // copied from std.testing.expectEqualInner
        const Tag = std.meta.Tag(@TypeOf(self));
        if (@as(Tag, self) != @as(Tag, other)) return false;
        return switch (self) {
            .identifier => std.mem.eql(u8, self.identifier, other.identifier),
            .string => std.mem.eql(u8, self.string, other.string),
            else => true,
        };
    }

    pub fn format(self: Token, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try switch (self) {
            .open_round => writer.print("(", .{}),
            .close_round => writer.print(")", .{}),
            .identifier => |x| writer.print("{s}", .{x}),
            .string => |x| writer.print("\"{s}\"", .{x}),
            .equal => writer.print("=", .{}),
            .comma => writer.print(",", .{}),
            .eof => writer.print("<EOF>", .{}),
        };
    }
};

const Tokenizer = struct {
    buffer: []const u8 = &.{},
    index: usize = 0,
    strings: *StringStorage,

    pub fn init(buffer: []const u8, strings: *StringStorage) Tokenizer {
        return .{ .buffer = buffer, .index = 0, .strings = strings };
    }

    pub fn deinit(self: *Tokenizer) void {
        self.* = undefined;
    }

    pub const Result = union(enum) {
        ok: TokenLoc,
        err: ErrLoc,
    };
    pub const TokenLoc = struct {
        token: Token,
        loc: usize,
    };
    pub const ErrLoc = struct {
        err: Err,
        loc: usize,
    };
    pub const Err = enum {
        empty,
        unterminated_string,
        bad_token,
    };

    pub fn next(self: *Tokenizer) error{OutOfMemory}!Result {
        const State = enum {
            start,
            identifier,
            string,
        };
        const Loc = struct {
            start: usize,
            end: usize,
        };

        var state: State = .start;
        var string: Loc = .{ .start = self.index, .end = self.index };

        while (self.index < self.buffer.len) {
            const c = self.buffer[self.index];
            const cpos = self.index;
            self.index += 1;

            switch (state) {
                .start => switch (c) {
                    'A'...'Z', 'a'...'z', '_', '.' => state = .identifier,
                    '"' => {
                        string.start = cpos;
                        state = .string;
                    },
                    '(' => return ok(.open_round, cpos),
                    ')' => return ok(.close_round, cpos),
                    '=' => return ok(.equal, cpos),
                    ',' => return ok(.comma, cpos),
                    '\n', '\r', '\t', ' ' => string.start = self.index, // skip whitespace
                    else => return err(.bad_token, cpos),
                },
                .string => switch (c) {
                    '"' => {
                        // string.start points to open quote
                        string.end = cpos;
                        const s = try self.strings.append(self.buffer[string.start + 1 .. string.end]);
                        return ok(.{ .string = s }, string.start);
                    },
                    else => continue,
                },
                .identifier => switch (c) {
                    'A'...'Z', 'a'...'z', '0'...'9', '-', '_', '.' => continue,
                    else => {
                        self.index -= 1; // backtrack
                        string.end = cpos;
                        const s = try self.strings.append(self.buffer[string.start..string.end]);
                        return ok(.{ .identifier = s }, string.start);
                    },
                },
            }
        }

        // end of input reached
        switch (state) {
            .start => return ok(.eof, self.index),
            .identifier => {
                string.end = self.index;
                const s = try self.strings.append(self.buffer[string.start..string.end]);
                return ok(.{ .identifier = s }, string.start);
            },
            .string => {
                return err(.unterminated_string, string.start);
            },
        }
    }

    pub fn back(self: *Tokenizer, loc: usize) void {
        self.index = loc;
    }

    fn ok(tok: Token, loc: usize) Result {
        return .{ .ok = .{ .token = tok, .loc = loc } };
    }
    fn err(e: Err, loc: usize) Result {
        return .{ .err = .{ .err = e, .loc = loc } };
    }
};

// -- parser -------------------------------------------------------

const Node = union(enum) {
    null,
    identifier: []const u8,
    string: []const u8,
    named_argument: NamedArgument,
    function_call: FunctionCall,

    pub fn eql(self: Node, other: Node) bool {
        // copied from std.testing.expectEqualInner
        const Tag = std.meta.Tag(@TypeOf(self));
        if (@as(Tag, self) != @as(Tag, other)) return false;
        return switch (self) {
            .identifier => std.mem.eql(u8, self.identifier, other.identifier),
            .string => std.mem.eql(u8, self.string, other.string),
            .named_argument => |na| na.eql(other.named_argument),
            .function_call => |fc| fc.eql(other.function_call),
            else => true,
        };
    }
    pub fn format(self: Node, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try switch (self) {
            .null => writer.print("null", .{}),
            .identifier => |s| writer.print("(identifier {s})", .{s}),
            .string => |s| writer.print("(string \"{s}\")", .{s}),
            .named_argument => |na| writer.print("(named-argument {})", .{na}),
            .function_call => |fc| writer.print("{}", .{fc}),
        };
    }
};

const NamedArgument = struct {
    name: []const u8,
    value: FunctionArg,

    pub fn eql(self: NamedArgument, other: NamedArgument) bool {
        const Tag = std.meta.Tag(@TypeOf(self.value));
        if (@as(Tag, self.value) != @as(Tag, other.value)) return false;
        if (!std.mem.eql(u8, self.name, other.name)) return false;
        return switch (self.value) {
            .identifier => |i| std.mem.eql(u8, i, other.value.identifier),
            .string => |s| std.mem.eql(u8, s, other.value.string),
            .function_call => |fc| fc.eql(other.value.function_call),
            else => true,
        };
    }

    pub fn format(self: NamedArgument, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("(named-argument {s} {})", .{ self.name, self.value });
    }
};
const FunctionCall = struct {
    name: []const u8,
    positional: []const FunctionArg,
    named: []const NamedArgument,

    pub fn eql(self: FunctionCall, other: FunctionCall) bool {
        if (!std.mem.eql(u8, self.name, other.name)) return false;
        return std.meta.eql(self.positional, other.positional) and std.meta.eql(self.named, other.named);
    }
    pub fn format(self: FunctionCall, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("(funcall {s}", .{self.name});
        for (self.positional) |fa| {
            try writer.print(" {}", .{fa});
        }
        for (self.named) |na| {
            try writer.print(" {}", .{na});
        }
        try writer.print(")", .{});
    }
};

const FunctionArg = union(enum) {
    null,
    identifier: []const u8,
    string: []const u8,
    function_call: FunctionCall,

    pub fn format(self: FunctionArg, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try switch (self) {
            .null => writer.print("null", .{}),
            .identifier => |s| writer.print("(identifer {s})", .{s}),
            .string => |s| writer.print("(string \"{s}\")", .{s}),
            .function_call => |fc| writer.print("{}", .{fc}),
        };
    }
};

const Parser = struct {
    alloc: Allocator,
    tokenizer: *Tokenizer,
    strings: *StringStorage,

    pub fn init(alloc: Allocator, tokenizer: *Tokenizer, strings: *StringStorage) Parser {
        return .{
            .alloc = alloc,
            .tokenizer = tokenizer,
            .strings = strings,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.* = undefined;
    }

    pub const Result = union(enum) {
        ok: NodeLoc,
        err: ErrLoc,
    };
    pub const NodeLoc = struct {
        node: Node,
        loc: usize,
    };
    pub const ErrLoc = struct {
        err: Err,
        loc: usize,
    };
    pub const Err = union(enum) {
        eof,

        // comma and close_round errors are returned from the start
        // state and handled by funcall_start.
        comma,
        close_round,

        expected_identifier,
        expected_argument,
        expected_funcall,
        unexpected_token,
        tokenizer_error: Tokenizer.Err,
    };

    pub fn next(self: *Parser) error{ OutOfMemory, TokenizeError, ParseError }!Result {
        const FuncallState = struct {
            name: struct { name: []const u8, loc: usize },
            positional: std.ArrayList(FunctionArg),
            named: std.ArrayList(NamedArgument),
        };
        const State = union(enum) {
            start,
            identifier: struct { name: []const u8, loc: usize },
            funcall_start: FuncallState,
            funcall_comma: FuncallState,
            funcall_identifier: struct {
                state: FuncallState,
                identifier: struct { name: []const u8, loc: usize },
            },
            funcall_identifier_equal: struct {
                state: FuncallState,
                identifier: struct { name: []const u8, loc: usize },
            },
        };
        var state: State = .start;

        while (true) {
            switch (state) {
                .start => {
                    const res = try self.tokenizer.next();
                    if (res == .err) return tokenizer_err(res.err);
                    switch (res.ok.token) {
                        .identifier => |s| state = .{
                            .identifier = .{
                                .name = s,
                                .loc = res.ok.loc,
                            },
                        },
                        .string => |s| return ok(.{ .string = s }, res.ok.loc),
                        .comma => return err(.comma, res.ok.loc),
                        .close_round => return err(.close_round, res.ok.loc),
                        .eof => return err(.eof, res.ok.loc),
                        .open_round,
                        .equal,
                        => return err(.expected_identifier, res.ok.loc),
                    }
                },
                .identifier => |s| {
                    const res = try self.tokenizer.next();
                    if (res == .err) return tokenizer_err(res.err);
                    switch (res.ok.token) {
                        .open_round => state = .{
                            .funcall_start = .{
                                .name = .{ .name = s.name, .loc = s.loc },
                                .positional = std.ArrayList(FunctionArg).init(self.alloc),
                                .named = std.ArrayList(NamedArgument).init(self.alloc),
                            },
                        },
                        .eof => return ok(.{ .identifier = s.name }, res.ok.loc),
                        else => return err(.unexpected_token, res.ok.loc),
                    }
                },
                .funcall_start => |*st| {
                    const res = try self.tokenizer.next();
                    if (res == .err) return tokenizer_err(res.err);
                    switch (res.ok.token) {
                        .identifier => |s| {
                            state = .{ .funcall_identifier = .{
                                .state = st.*,
                                .identifier = .{ .name = s, .loc = res.ok.loc },
                            } };
                        },
                        .string => |s| try st.positional.append(.{ .string = s }),
                        .comma => state = .{ .funcall_comma = st.* },
                        .close_round => {
                            // end of funcall
                            return ok(.{ .function_call = .{
                                .name = st.name.name,
                                .positional = try st.positional.toOwnedSlice(),
                                .named = try st.named.toOwnedSlice(),
                            } }, st.name.loc);
                        },
                        else => return err(.unexpected_token, res.ok.loc),
                    }
                },
                .funcall_comma => |*st| {
                    const res = try self.tokenizer.next();
                    if (res == .err) return tokenizer_err(res.err);

                    // if we see a second comma, infer a null
                    // positional argument. Otherwise, restart in
                    // funcall_start state.
                    switch (res.ok.token) {
                        .comma => {
                            try st.positional.append(.null);
                            state = .{ .funcall_start = st.* };
                        },
                        else => {
                            self.tokenizer.back(res.ok.loc);
                            state = .{ .funcall_start = st.* };
                        },
                    }
                },
                .funcall_identifier => |*fist| {
                    const res = try self.tokenizer.next();
                    if (res == .err) return tokenizer_err(res.err);
                    switch (res.ok.token) {
                        .comma => {
                            try fist.state.positional.append(.{ .identifier = fist.identifier.name });
                            state = .{ .funcall_start = fist.state };
                        },
                        .equal => {
                            state = .{ .funcall_identifier_equal = .{
                                .state = fist.state,
                                .identifier = .{
                                    .name = fist.identifier.name,
                                    .loc = fist.identifier.loc,
                                },
                            } };
                        },
                        .open_round => {
                            // funcall as positional argument.
                            // backtrack and parse expression
                            self.tokenizer.back(fist.identifier.loc);

                            const inner = try self.next();
                            if (inner == .err) return inner;

                            const fc: FunctionCall =
                                switch (inner.ok.node) {
                                .function_call => |fc| fc,
                                else => return err(.expected_funcall, res.ok.loc),
                            };

                            try fist.state.positional.append(.{ .function_call = fc });
                            state = .{ .funcall_start = fist.state };
                        },
                        else => return err(.unexpected_token, res.ok.loc),
                    }
                },
                .funcall_identifier_equal => |*fiest| {
                    // parse next expression
                    const res = try self.next();
                    if (res == .err) return res;

                    var na: NamedArgument = .{
                        .name = fiest.identifier.name,
                        .value = .null,
                    };
                    na.value = switch (res.ok.node) {
                        .null => .null,
                        .identifier => |s| .{ .identifier = s },
                        .string => |s| .{ .string = s },
                        .function_call => |fc| .{ .function_call = fc },
                        else => return err(.expected_argument, res.ok.loc),
                    };

                    try fiest.state.named.append(na);
                    state = .{ .funcall_start = fiest.state };
                },
            }
        }
    }

    fn ok(node: Node, loc: usize) Result {
        return .{ .ok = .{ .node = node, .loc = loc } };
    }
    fn err(e: Err, loc: usize) Result {
        return .{ .err = .{ .err = e, .loc = loc } };
    }
    fn tokenizer_err(tok_err: Tokenizer.ErrLoc) Result {
        return .{ .err = .{ .err = .{ .tokenizer_error = tok_err.err }, .loc = tok_err.loc } };
    }
};

// -- tests --------------------------------------------------------

test "tokenize" {
    const alloc = std.testing.allocator;
    const source =
        \\c()
        \\
    ;

    var strings = try StringStorage.init(alloc, std.heap.page_allocator);
    defer strings.deinit();

    var tokenizer = Tokenizer.init(source, &strings);
    defer tokenizer.deinit();

    try tokenizeExpect(alloc, &tokenizer, &.{
        .{ .identifier = "c" },
        .open_round,
        .close_round,
    });
}

test "tokenize 2" {
    const alloc = std.testing.allocator;
    const source =
        \\        c(
        \\    person("Caio", "Lente", , "clente@abj.org.br", role = c("aut", "cre"),
        \\           comment = c(ORCID = "0000-0001-8473-069X")),
        \\  )
        \\
        \\
    ;
    var strings = try StringStorage.init(alloc, std.heap.page_allocator);
    defer strings.deinit();

    var tokenizer = Tokenizer.init(source, &strings);
    defer tokenizer.deinit();

    try tokenizeExpect(alloc, &tokenizer, &.{
        .{ .identifier = "c" },
        .open_round,
        .{ .identifier = "person" },
        .open_round,
        .{ .string = "Caio" },
        .comma,
        .{ .string = "Lente" },
        .comma,
        .comma,
        .{ .string = "clente@abj.org.br" },
        .comma,
        .{ .identifier = "role" },
        .equal,
        .{ .identifier = "c" },
        .open_round,
        .{ .string = "aut" },
        .comma,
        .{ .string = "cre" },
        .close_round,
        .comma,
        .{ .identifier = "comment" },
        .equal,
        .{ .identifier = "c" },
        .open_round,
        .{ .identifier = "ORCID" },
        .equal,
        .{ .string = "0000-0001-8473-069X" },
        .close_round,
        .close_round,
        .comma,
        .close_round,
    });
}

test "parse" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const source =
        \\     c(
        \\  person("Caio", "Lente", , "clente@abj.org.br", role = c("aut", "cre"),
        \\           comment = c(ORCID = "0000-0001-8473-069X")),
        \\  )
        \\
        \\
    ;
    var strings = try StringStorage.init(alloc, std.heap.page_allocator);
    defer strings.deinit();

    var tokenizer = Tokenizer.init(source, &strings);
    defer tokenizer.deinit();

    var parser = Parser.init(arena.allocator(), &tokenizer, &strings);
    defer parser.deinit();

    try doParseDebug(&parser);

    // Outputs:
    // RESULT: 5: (funcall c (funcall person (string "Caio") (string "Lente") null (string "clente@abj.org.br") (named-argument role (funcall c (string "aut") (string "cre"))) (named-argument comment (funcall c (named-argument ORCID (string "0000-0001-8473-069X"))))))
    // EOF: 142

}

test "parse 1" {
    const alloc = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    const source = "c()";

    var strings = try StringStorage.init(alloc, std.heap.page_allocator);
    defer strings.deinit();

    var tokenizer = Tokenizer.init(source, &strings);
    defer tokenizer.deinit();

    var parser = Parser.init(arena.allocator(), &tokenizer, &strings);
    defer parser.deinit();

    try doParseDebug(&parser);
}

fn doParseDebug(parser: *Parser) !void {
    while (true) {
        const res = try parser.next();
        switch (res) {
            .err => |e| {
                if (e.err == .eof) {
                    std.debug.print("EOF: {}\n", .{e.loc});
                    return;
                }
                std.debug.print("ERROR: {}: {}\n", .{ e.loc, e.err });
                return;
            },
            .ok => |ok| {
                std.debug.print("RESULT: {}: {}\n", .{ ok.loc, ok.node });
            },
        }
    }
}

fn tokenizeExpect(alloc: Allocator, tokenizer: *Tokenizer, expect: []const Token) !void {
    var toks = std.ArrayList(Token).init(alloc);
    defer toks.deinit();
    while (true) {
        switch (try tokenizer.next()) {
            .ok => |token_loc| {
                switch (token_loc.token) {
                    .eof => break,
                    else => {
                        try toks.append(token_loc.token);
                        // std.debug.print("Token: {}\n", .{token_loc.token});
                    },
                }
            },
            .err => |e| {
                std.debug.print("Unexpected tokenize error: {}\n", .{e});
                return error.TokenizeError;
            },
        }
    }

    try expectTokens(expect, toks.items);
}

fn expectTokens(expect: []const Token, actual: []const Token) !void {
    if (expect.len != actual.len) return error.UnequalLengths;

    // langref: lengths must be equal otherwise detectable illegal
    // behaviour, so the above if test is not strictly needed for a
    // test function
    for (expect, actual) |e, a| {
        if (!e.eq(a)) {
            std.debug.print("Expected: {}, actual: {}\n", .{ e, a });
            return error.ExpectFailed;
        }
    }
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const StringStorage = @import("string_storage.zig").StringStorage;
