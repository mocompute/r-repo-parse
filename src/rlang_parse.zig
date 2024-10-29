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
                        string.start = self.index; // skip the open quote
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
                        string.end = cpos;
                        const s = try self.strings.append(self.buffer[string.start..string.end]);
                        return ok(.{ .string = s }, cpos);
                    },
                    else => continue,
                },
                .identifier => switch (c) {
                    'A'...'Z', 'a'...'z', '0'...'9', '-', '_', '.' => continue,
                    else => {
                        self.index -= 1; // backtrack
                        string.end = cpos;
                        const s = try self.strings.append(self.buffer[string.start..string.end]);
                        return ok(.{ .identifier = s }, cpos);
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
};

const NamedArgument = struct {
    name: []const u8,
    value: union(enum) {
        null,
        identifier: []const u8,
        string: []const u8,
        function_call: FunctionCall,
    },

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
};
const FunctionCall = struct {
    name: []const u8,
    positional: []const FunctionArg,
    named: []const NamedArgument,

    pub fn eql(self: FunctionCall, other: FunctionCall) bool {
        if (!std.mem.eql(u8, self.name, other.name)) return false;
        return std.meta.eql(self.positional, other.positional) and std.meta.eql(self.named, other.named);
    }
};

const FunctionArg = union(enum) {
    null,
    identifier: []const u8,
    string: []const u8,
    function_call: FunctionCall,
};

const Parser = struct {
    alloc: Allocator,
    tokenizer: *Tokenizer,

    pub fn init(alloc: Allocator, tokenizer: *Tokenizer, strings: *StringStorage) Parser {
        return .{
            .alloc = alloc,
            .tokenizer = tokenizer,
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
        expected_identifier,
        expected_argument,
        unexpected_token,
        tokenizer_error: Tokenizer.Err,
    };

    pub fn parse(self: *Parser) error{ OutOfMemory, TokenizeError, ParseError }!Result {
        const State = union(enum) {
            start,
            open_round,
            identifier: []const u8,
            funcall: []const u8,
            identifier_equal,
            value,
        };
        var state: State = start;

        var positional = std.ArrayList(FunctionArg).init(self.alloc);
        defer positional.deinit();
        var named = std.ArrayList(NamedArgument).init(self.alloc);
        defer named.deinit();

        while (true) {
            const res = try self.tokenizer.next();
            if (res == .err) return err(.{ .tokenizer_error = res.err.err }, res.err.loc);
            const tok = res.ok.token;
            const loc = res.ok.loc;

            switch (state) {
                .start => switch (tok) {
                    .identifier => |s| state = .{ .identifier = s },
                    .open_round, .close_round, .string, .equal, .comma => return err(.expected_identifier, loc),
                    .eof => return err(.eof, loc),
                },
                .identifier => |s| switch (tok) {
                    .open_round => state = .{ .funcall = s }, // it's a funcall
                    .eof => return err(.eof, loc),
                    else => return err(unexpected_token, loc);
                },
                .funcall => |s| switch(tok) {
                    // FIXME
                }
            }
        }

        return switch (res.ok.token) {
            .open_round, .close_round, .equal, .comma => err(.expected_identifier, res.ok.loc),
            .identifier => |name| {
                const fc = try self.parseFunctionCall(name);
                return ok(.{ .function_call = fc }, res.ok.loc);
            },
            .string => |s| ok(.{ .string = s }, res.ok.loc),
            .eof => err(.eof, res.ok.loc),
        };
    }

    fn ok(node: Node, loc: usize) Result {
        return .{ .ok = .{ .node = node, .loc = loc } };
    }
    fn err(e: Err, loc: usize) Result {
        return .{ .err = .{ .err = e, .loc = loc } };
    }

    fn parseFunctionCall(self: *Parser, name: []const u8) !Result {
        const State = enum {
            start,
            open_round,
            identifier,
            identifier_equal,
            value,
        };
        var state: State = start;

        var positional = std.ArrayList(FunctionArg).init(self.alloc);
        defer positional.deinit();
        var named = std.ArrayList(NamedArgument).init(self.alloc);
        defer named.deinit();

        while (true) {
            const res = try self.tokenizer.next();
            if (res == .err) return err(.{ .tokenizer_error = res.err.err }, res.err.loc);
            const tok = res.ok.token;
            const loc = res.ok.loc;

            var funcall: FunctionCall = .{ .name = name, .positional = .{}, .named = .{} };

            switch (state) {
                .start => switch (tok) {
                    .open_round => state = .open_round,
                    .close_round => {
                        funcall.positional = positional.toOwnedSlice();
                        funcall.named = named.toOwnedSlice();
                        return ok(.{ .function_call = funcall }, loc);
                    },
                },
            }
        }

        try self.expectToken(.open_round);

        while (true) {
            const res = try self.tokenizer.next();
            switch (res) {
                .ok => |tok_loc| {
                    switch (tok_loc.token) {
                        .open_round => {
                            return err(.expected_argument, tok_loc.loc);
                        },
                        close_round => {},
                        identifier => |s| {},
                        string => |s| {},
                        equal => {},
                        comma => {},
                        eof => {},
                    }
                },
                .err => |err_loc| return err(.{ .tokenizer_error = err_loc.err }, err_loc.loc),
            }
        }

        // FIXME not implemented

        try self.expectToken(.close_round);

        return .{
            .name = name,
            .positional = &.{},
            .named = &.{},
        };
    }

    fn parseIdentifier(self: *Parser) ![]const u8 {
        switch (try self.tokenizer.next()) {
            .ok => |tok_loc| {
                switch (tok_loc.token) {
                    .identifier => |name| return name,
                    else => {
                        std.debug.print("Unexpected token at loc {}: {}\n", .{
                            tok_loc.loc,
                            tok_loc.token,
                        });
                        return error.ParseError;
                    },
                }
            },
            .err => |err_loc| {
                std.debug.print("Tokenizer error at loc {}: {}\n", .{ err_loc.loc, err_loc.err });
                return error.ParseError;
            },
        }
    }

    fn expectToken(self: *Parser, tok: Token) !void {
        const res = try self.tokenizer.next();
        if (res == .ok and res.ok.token.eq(tok)) return;

        std.debug.print("Unexpected token: {}\n", .{res});
        return error.ParseError;
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

    var parser = Parser.init(alloc, source, &strings);
    defer parser.deinit();

    // FIXME incomplete, not implemented
    try parseExpect(&parser, .{ .function_call = .{
        .name = "c",
        .positional = &.{.{
            .function_call = .{
                .name = "person",
                .positional = &.{
                    .{ .string = "Caio" },
                    .{ .string = "Lente" },
                    .null,
                    .{ .string = "clente@abj.org.br" },
                },
                .named = &.{},
            },
        }},
        .named = &.{},
    } });
}

test "parse 1" {
    const alloc = std.testing.allocator;
    const source = "c()";

    var strings = try StringStorage.init(alloc, std.heap.page_allocator);
    defer strings.deinit();

    var parser = Parser.init(alloc, source, &strings);
    defer parser.deinit();

    try parseExpect(&parser, .{ .function_call = .{
        .name = "c",
        .positional = &.{},
        .named = &.{},
    } });
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

fn parseExpect(parser: *Parser, expect: Node) !void {
    const res = try parser.parse();
    switch (res) {
        .ok => |node_loc| {
            const node = node_loc.node;
            try std.testing.expect(expect.eql(node));
        },
        .err => |err_loc| {
            std.debug.print("Unexpected parse error at loc {}: {}\n", .{ err_loc.loc, err_loc.err });
            return error.ParseError;
        },
    }
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const StringStorage = @import("string_storage.zig").StringStorage;
