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

alloc: Allocator,

const Person = struct {
    role: Role,
    family: []const u8,
    given: []const u8,
    first: []const u8,
    middle: []const u8,
    last: []const u8,
    email: []const u8,
    comment: []const u8,
    orcid: []const u8,

    pub fn fromFunctionCall(fc: FunctionCall) Person {}
};

const Role = enum {
    author,
    compiler,
    contributor,
    copyright_holder,
    creator,
    thesis_advisor,
    translator,

    pub fn fromString(s: []const u8) Role {
        const eql = std.ascii.eqlIgnoreCase;
        if (eql(s, "aut")) {
            return .author;
        } else if (eql(s, "com")) {
            return .compiler;
        } else if (eql(s, "ctb")) {
            return .contributor;
        } else if (eql(s, "cph")) {
            return .copyright_holder;
        } else if (eql(s, "cre")) {
            return .creator;
        } else if (eql(s, "ths")) {
            return .thesis_advisor;
        } else if (eql(s, "trl")) {
            return .translator;
        } else unreachable;
    }
};

pub fn init(alloc: Allocator) Authors {
    return .{
        .alloc = alloc,
    };
}

pub fn deinit(self: *Authors) void {
    self.* = undefined;
}

pub fn read(self: *Authors, source: []const u8, strings: *StringStorage) !void {

    // arena for the RParser
    var arena = std.heap.ArenaAllocator.init(self.alloc);
    defer arena.deinit();

    // parse DCF
    var parser = Parser.init(self.alloc, strings);
    defer parser.deinit();
    try parser.parse(source);

    // iterate through parsed stanzas.

    const nodes = parser.nodes.items;
    var index: usize = 0;
    var package_name: ?[]const u8 = null;
    while (true) : (index += 1) {
        switch (nodes[index]) {
            .eof => break,
            .stanza_end => {
                package_name = null;
            },
            .field => |field| {
                if (std.ascii.eqlIgnoreCase("package", field.name)) {
                    package_name = try parsePackageName(nodes, &index, strings);
                } else if (std.ascii.eqlIgnoreCase("authors@r", field.name)) {
                    assert(package_name != null);

                    const field_source = b: {
                        index += 1;
                        switch (nodes[index]) {
                            .string_node => |s| break :b s,
                            else => unreachable,
                        }
                    };

                    var rtokenizer = RTokenizer.init(field_source.value, strings);
                    defer rtokenizer.deinit();
                    var rparser = RParser.init(arena.allocator(), &rtokenizer, strings);
                    defer rparser.deinit();

                    switch (try rparser.next()) {
                        .ok => |ok| {
                            switch (ok.node) {
                                .function_call => |fc| {
                                    std.debug.print("Parsed: {}\n", .{fc});

                                    // outer function can be c() or person()
                                    if (std.mem.eql(u8, "c", fc.name)) {
                                        0;
                                    } else if (std.mem.eql(u8, "person", fc.name)) {
                                        0;
                                    } else unreachable;
                                },
                                .function_arg => return error.RParseExpectedFunctionCall,
                            }
                        },
                        .err => return error.RParseError,
                    }
                }
            },
            else => continue,
        }
    }
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

test "Authors" {
    const alloc = std.testing.allocator;
    const source =
        \\Package: renv
        \\Type: Package
        \\Title: Project Environments
        \\Version: 1.0.7.9000
        \\Authors@R: c(
        \\    person("Kevin", "Ushey", role = c("aut", "cre"), email = "kevin@rstudio.com",
        \\           comment = c(ORCID = "0000-0003-2880-7407")),
        \\    person("Hadley", "Wickham", role = c("aut"), email = "hadley@rstudio.com",
        \\           comment = c(ORCID = "0000-0003-4757-117X")),
        \\    person("Posit Software, PBC", role = c("cph", "fnd"))
        \\    )
        \\Description: A dependency management toolkit for R. Using 'renv', you can create
        \\    and manage project-local R libraries, save the state of these libraries to
        \\    a 'lockfile', and later restore your library as required. Together, these
        \\    tools can help make your projects more isolated, portable, and reproducible.
        \\License: MIT + file LICENSE
        \\URL: https://rstudio.github.io/renv/, https://github.com/rstudio/renv
        \\BugReports: https://github.com/rstudio/renv/issues
        \\Imports: utils
        \\Suggests: BiocManager (> 0.1), cli, covr, cpp11, devtools, gitcreds, jsonlite, jsonvalidate, knitr
        \\    (> 2.0),
        \\    miniUI, packrat, pak, R6, remotes, reticulate, rmarkdown, rstudioapi, shiny, testthat,
        \\    uuid, waldo, yaml, webfakes
        \\Encoding: UTF-8
        \\RoxygenNote: 7.3.2
        \\Roxygen: list(markdown = TRUE)
        \\VignetteBuilder: knitr
        \\Config/Needs/website: tidyverse/tidytemplate
        \\Config/testthat/edition: 3
        \\Config/testthat/parallel: true
        \\Config/testthat/start-first: bioconductor,python,install,restore,snapshot,retrieve,remotes
    ;

    var strings = try StringStorage.init(alloc, std.heap.page_allocator);
    defer strings.deinit();

    var authors = Authors.init(alloc);
    defer authors.deinit();

    try authors.read(source, &strings);
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const StringStorage = @import("string_storage.zig").StringStorage;

const parse = @import("parse.zig");
const Parser = parse.Parser;

const rlang_parse = @import("rlang_parse.zig");
const RTokenizer = rlang_parse.Tokenizer;
const RParser = rlang_parse.Parser;
const FunctionCall = rlang_parse.FunctionCall;

const Authors = @This();
const assert = std.debug.assert;
