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
db: AuthorsDB,

pub fn init(alloc: Allocator) Authors {
    return .{
        .alloc = alloc,
        .db = try AuthorsDB.init(alloc),
    };
}

pub fn deinit(self: *Authors) void {
    self.db.deinit();
    self.* = undefined;
}

const AuthorsDB = struct {
    alloc: Allocator,
    person_ids: Storage(PersonId, PersonId),
    person_strings: PersonAttributes([]const u8),
    person_roles: PersonAttributes(Role),
    attribute_names: Storage([]const u8, AttributeId),
    package_names: Storage([]const u8, PackageId),

    /// Prefer to use ArenaAllocator as the parser is leaky.
    pub fn init(alloc: Allocator) !AuthorsDB {
        return .{
            .alloc = alloc,
            .person_ids = Storage(PersonId, PersonId).init(alloc),
            .person_strings = try PersonAttributes([]const u8).init(alloc),
            .person_roles = try PersonAttributes(Role).init(alloc),
            .attribute_names = Storage([]const u8, AttributeId).init(alloc),
            .package_names = Storage([]const u8, PackageId).init(alloc),
        };
    }
    pub fn deinit(self: *AuthorsDB) void {
        self.person_ids.deinit();
        self.person_strings.deinit();
        self.person_roles.deinit();
        self.attribute_names.deinit();
        self.package_names.deinit();
        self.* = undefined;
    }
    pub fn debugPrint(self: AuthorsDB) void {
        std.debug.print("\nAttributes:\n", .{});
        for (self.attribute_names.data.items, 0..) |x, id| {
            std.debug.print("  {}: {s}\n", .{ id, x });
        }

        std.debug.print("\nPackages:\n", .{});
        for (self.package_names.data.items, 0..) |x, id| {
            std.debug.print("  {}: {s}\n", .{ id, x });
        }

        std.debug.print("\nPersons:\n", .{});
        std.debug.print("  Count: {}\n", .{self.person_ids._next});

        std.debug.print("\nPerson strings:\n", .{});
        for (self.person_strings.data.data.items, 0..) |x, id| {
            std.debug.print(
                "  {}: (package_id {}) (person_id {}) (attr_id {}) (value {s})\n",
                .{
                    id,
                    x.package_id,
                    x.person_id,
                    x.attribute_id,
                    x.value,
                },
            );
        }

        std.debug.print("\nPerson roles:\n", .{});
        for (self.person_roles.data.data.items, 0..) |x, id| {
            std.debug.print(
                "  {}: (package_id {}) (person_id {}) (attr_id {}) (value {})\n",
                .{
                    id,
                    x.package_id,
                    x.person_id,
                    x.attribute_id,
                    x.value,
                },
            );
        }
    }

    pub fn nextPersonId(self: *AuthorsDB) PersonId {
        return self.person_ids.nextId();
    }

    /// Leaky, prefer to use an ArenaAllocator.
    pub fn addFromFunctionCall(self: *AuthorsDB, fc: FunctionCall, package_name: []const u8) !void {
        assert(std.mem.eql(u8, "person", fc.name));
        const eql = std.ascii.eqlIgnoreCase;

        const package_id = self.package_names.lookupString(package_name) orelse b: {
            const id = self.package_names.nextId();
            try self.package_names.put(id, package_name);
            break :b id;
        };

        // person (given = NULL, family = NULL, middle = NULL, email = NULL,
        //     role = NULL, comment = NULL, first = NULL, last = NULL)

        // each person function allocates a new person id. we don't
        // deduplicate because anyway how?
        const person_id = self.nextPersonId();

        for (fc.positional, 1..) |fa, pos| {
            var attr_id: AttributeId = undefined;
            var string_value: ?[]const u8 = null;

            switch (pos) {
                1 => {
                    attr_id = try self.attributeId("given");
                    string_value = switch (fa) {
                        .string => |s| s,
                        else => null,
                    };
                },
                2 => {
                    attr_id = try self.attributeId("family");
                    string_value = switch (fa) {
                        .string => |s| s,
                        else => null,
                    };
                },
                3 => {
                    attr_id = try self.attributeId("middle");
                    string_value = switch (fa) {
                        .string => |s| s,
                        else => null,
                    };
                },
                4 => {
                    attr_id = try self.attributeId("email");
                    string_value = switch (fa) {
                        .string => |s| s,
                        else => null,
                    };
                },
                5 => {
                    attr_id = try self.attributeId("role");
                    string_value = switch (fa) {
                        .string => |s| s,
                        else => null,
                    };
                },
                6 => {
                    attr_id = try self.attributeId("comment");
                    string_value = switch (fa) {
                        .string => |s| s,
                        else => null,
                    };
                },
                7 => {
                    attr_id = try self.attributeId("first");
                    string_value = switch (fa) {
                        .string => |s| s,
                        else => null,
                    };
                },
                8 => {
                    attr_id = try self.attributeId("last");
                    string_value = switch (fa) {
                        .string => |s| s,
                        else => null,
                    };
                },
                else => unreachable,
            }

            if (string_value) |s| {
                try self.putNewString(package_id, person_id, attr_id, s);
            }
        } // positional arguments

        for (fc.named) |na| {
            var attr_id: AttributeId = undefined;

            // person (given = NULL, family = NULL, middle = NULL, email = NULL,
            //     role = NULL, comment = NULL, first = NULL, last = NULL)
            if (eql("comment", na.name)) {
                attr_id = try self.attributeId("comment");
                switch (na.value) {
                    .string => |s| {
                        try self.putNewString(package_id, person_id, attr_id, s);
                    },
                    .function_call => |comment_fc| {
                        if (eql("c", comment_fc.name)) {
                            for (comment_fc.named) |comment_na| {
                                attr_id = try self.attributeId(comment_na.name);
                                switch (comment_na.value) {
                                    .string => |comment_s| {
                                        try self.putNewString(package_id, person_id, attr_id, comment_s);
                                    },
                                    else => unreachable,
                                }
                            }
                        }
                    },
                    else => unreachable,
                }
            } else if (eql("role", na.name)) {
                attr_id = try self.attributeId("role");
                switch (na.value) {
                    .string => |s| try self.putNewRole(package_id, person_id, attr_id, Role.fromString(s)),
                    .function_call => |role_fc| {
                        if (eql("c", role_fc.name)) {
                            for (role_fc.positional) |fa| {
                                switch (fa) {
                                    .string => |s| try self.putNewRole(package_id, person_id, attr_id, Role.fromString(s)),
                                    else => unreachable,
                                }
                            }
                            for (role_fc.named) |_| {
                                unreachable;
                            }
                        }
                    },
                    else => unreachable,
                }
            } else {
                attr_id = try self.attributeId(na.name);
                switch (na.value) {
                    .string => |s| try self.putNewString(package_id, person_id, attr_id, s),
                    else => {
                        std.debug.print("ERROR: unexpected function call in named argument: {s}\n", .{package_name});
                    },
                }
            }
        } // named arguments
    }

    fn attributeId(self: *AuthorsDB, name: []const u8) !AttributeId {
        return self.attribute_names.lookupString(name) orelse b: {
            const id = self.attribute_names.nextId();
            try self.attribute_names.put(id, name);
            break :b id;
        };
    }

    fn putNewString(
        self: *AuthorsDB,
        package_id: PackageId,
        person_id: PersonId,
        attribute_id: AttributeId,
        value: []const u8,
    ) !void {
        try self.person_strings.put(self.person_strings.data.nextId(), .{
            .package_id = package_id,
            .person_id = person_id,
            .attribute_id = attribute_id,
            .value = value,
        });
    }

    fn putNewRole(
        self: *AuthorsDB,
        package_id: PackageId,
        person_id: PersonId,
        attribute_id: AttributeId,
        value: Role,
    ) !void {
        try self.person_roles.put(self.person_roles.data.nextId(), .{
            .package_id = package_id,
            .person_id = person_id,
            .attribute_id = attribute_id,
            .value = value,
        });
    }
};

fn PersonAttribute(comptime T: type) type {
    return struct {
        package_id: PackageId,
        person_id: PersonId,
        attribute_id: AttributeId,
        value: T,
    };
}

/// Storage for PersonAttributes
fn PersonAttributes(comptime T: type) type {
    return struct {
        /// field may be accessed directly for read-only operations, but
        /// use methods for destructive ops.
        data: Storage(ValueType, IdType),

        const ValueType = PersonAttribute(T);
        const IdType = PersonAttributeId;

        pub fn init(alloc: Allocator) !@This() {
            return .{
                .data = Storage(ValueType, IdType).init(alloc),
            };
        }
        pub fn deinit(self: *@This()) void {
            self.data.deinit();
            self.* = undefined;
        }
        pub fn put(self: *@This(), id: IdType, value: ValueType) !void {
            try self.data.put(id, value);
        }

        /// Return next record matching person_id, starting at id
        /// from_id. From_id will be updated to point to record after
        /// the one returned.
        pub fn next_attribute(self: @This(), person_id: PersonId, from_id: *IdType) ?PersonAttribute {
            var index = from_id.*;
            const items = self.data.data.items;
            while (index < items.len) : (index += 1) {
                if (items[index].person_id == person_id) {
                    from_id.* = index + 1;
                    return items[index];
                }
            }
            return null;
        }
    };
}

fn Storage(comptime T: type, comptime IdType: type) type {
    return struct {
        data: std.ArrayList(T),
        _next: IdType = 0,

        pub fn init(alloc: Allocator) @This() {
            return .{
                .data = std.ArrayList(T).init(alloc),
            };
        }
        pub fn deinit(self: *@This()) void {
            self.data.deinit();
            self.* = undefined;
        }
        pub fn nextId(self: *@This()) IdType {
            assert(self._next != std.math.maxInt(IdType));
            const out = self._next;
            self._next += 1;
            return out;
        }
        pub fn put(self: *@This(), id: IdType, value: T) !void {
            assert(id != std.math.maxInt(IdType));
            const new_len = id + 1;
            if (self.data.items.len < new_len)
                try self.data.resize(new_len);

            self.data.items[id] = value;
        }
        pub fn get(self: @This(), id: IdType) !T {
            if (id < self.data.items.len) return self.data.items[id];
            return error.OutOfRange;
        }
        pub fn lookup(self: @This(), value: T) ?IdType {
            var index: IdType = 0;
            for (self.data.items) |x| {
                if (std.meta.eql(x, value)) return index;
                index += 1;
            }
            return null;
        }
        pub fn lookupString(self: @This(), value: T) ?IdType {
            var index: IdType = 0;
            for (self.data.items) |x| {
                if (std.mem.eql(u8, x, value)) return index;
                index += 1;
            }
            return null;
        }
    };
}

const AttributeId = u16;
const PersonAttributeId = u32;
const PersonId = u32;
const PackageId = u32;

const Role = enum {
    author,
    compiler,
    contributor,
    copyright_holder,
    creator,
    thesis_advisor,
    translator,
    contractor,
    data_contributor,
    funder,
    reviewer,

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
        } else if (eql(s, "ctr")) {
            return .contractor;
        } else if (eql(s, "dtc")) {
            return .translator;
        } else if (eql(s, "fnd")) {
            return .translator;
        } else if (eql(s, "rev")) {
            return .reviewer;
        } else {
            std.debug.print("error: got unknown role: {s}\n", .{s});
            unreachable;
        }
    }
};

const Comment = union(enum) {
    string: []const u8,
    map: std.StringArrayHashMap([]const u8),
};

pub fn read(self: *Authors, source: []const u8, strings: *StringStorage) !void {
    const eql = std.ascii.eqlIgnoreCase;

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
                                        for (fc.positional) |fa| {
                                            switch (fa) {
                                                .function_call => |c_fc| {
                                                    if (eql("person", c_fc.name)) {
                                                        try self.db.addFromFunctionCall(c_fc, package_name.?);
                                                    } else unreachable;
                                                },
                                                else => unreachable,
                                            }
                                        }
                                    } else if (eql("person", fc.name)) {
                                        try self.db.addFromFunctionCall(fc, package_name.?);
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
        \\Authors@R: person('Pierre-Yves', 'de Müllenheim', email = 'pydemull@uco.fr', role = c('cre', 'aut'), comment = c(ORCID = "0000-0001-9157-7371"))
        \\Authors@R: c(person(given = "Sy Han", family = "Chiou", email = "schiou@smu.edu", role = c("aut", "cre")),
        \\           person(given = "Sangwook", family = "Kang", role = "aut"),
        \\           person(given = "Jun", family = "Yan", role = "aut"))
        \\Authors@R: c(person(("Atanu"), "Bhattacharjee",
        \\                    email="atanustat@gmail.com",
        \\                    role=c("aut", "cre","ctb")),
        \\               person(("Gajendra Kumar"), "Vishwakarma", role=c("aut","ctb")),
        \\               person(("Pragya"), "Kumari", role=c("aut","ctb")))
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

    authors.db.debugPrint();
}

// test "read authors from PACKAGES.gz" {
//     const mos = @import("mos");
//     const alloc = std.testing.allocator;
//     const path = "PACKAGES-full.gz";
//     std.fs.cwd().access(path, .{}) catch return;

//     const source: ?[]const u8 = try mos.file.readFileMaybeGzip(alloc, path);
//     try std.testing.expect(source != null);
//     defer if (source) |s| alloc.free(s);

//     if (source) |source_| {
//         var strings = try StringStorage.init(alloc, std.heap.page_allocator);
//         defer strings.deinit();

//         var authors = Authors.init(alloc);
//         defer authors.deinit();

//         var timer = try std.time.Timer.start();
//         try authors.read(source_, &strings);
//         std.debug.print("Parse authors = {}ms\n", .{@divFloor(timer.lap(), 1_000_000)});

//         authors.db.debugPrint();
//     }
// }

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
