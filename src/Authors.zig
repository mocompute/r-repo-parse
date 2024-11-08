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

pub const LogType = enum { info, warn, err };
pub const LogTag = union(LogType) {
    info: enum {
        no_authors_at_r,
    },
    warn: enum {
        no_package,
        no_function,
        no_person,
        unknown_role_code,
    },
    err: enum {
        authors_at_r_wrong_type,
        rlang_parse_error,
        expected_string_in_role,
        named_argument_in_role,
    },
};
pub const LogItem = struct {
    message: []const u8 = "",
    loc: usize = 0,
    extra: union(enum) {
        none: void,
        string: []const u8,
        rlang_parse_err: RParser.ErrLoc,
    } = .none,
    tag: LogTag,

    pub fn format(self: LogItem, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try switch (self.tag) {
            .info => |x| switch (x) {
                .no_authors_at_r => writer.print("info: Package '{s'}: no Authors@R field", .{self.message}),
            },
            .warn => |x| switch (x) {
                .no_package => writer.print("warn: no package found in stanza", .{}),
                .no_function => writer.print("warn: Package '{s}': expected c() or person() function call in Authors@R field", .{self.message}),
                .no_person => writer.print("warn: Package '{s}': expected person() in Authors@R field", .{self.message}),
                .unknown_role_code => writer.print("warn: Package '{s}': unknown role code '{s}'", .{ self.message, self.extra.string }),
            },
            .err => |x| switch (x) {
                .authors_at_r_wrong_type => writer.print("error: Package '{s}': Authors@R parsed to wrong type", .{self.message}),
                .rlang_parse_error => writer.print("error: Package '{s}' R lang parser error: {} at location {}", .{ self.message, self.extra.rlang_parse_err.err, self.extra.rlang_parse_err.loc }),
                .expected_string_in_role => writer.print("error: Package '{s}': role must be a string or identifier.", .{self.message}),
                .named_argument_in_role => writer.print("error: Package '{s}': named argument not supported in role vector.", .{self.message}),
            },
        };
    }
};

const LogItems = std.ArrayList(LogItem);

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
    pub fn debugPrintInfo(self: *const AuthorsDB) void {
        std.debug.print("Packages with Authors@R: {}\n", .{self.package_names.data.items.len});
        std.debug.print("Persons: {}\n", .{self.person_ids._next});
        std.debug.print("String attributes: {}\n", .{self.person_strings.data.data.items.len});
        std.debug.print("Role attributes: {}\n", .{self.person_roles.data.data.items.len});
    }
    pub fn debugPrint(self: *const AuthorsDB) void {
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
    pub fn addFromFunctionCall(self: *AuthorsDB, fc: FunctionCall, package_name: []const u8, log: *LogItems) !void {
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
                else => {
                    std.debug.print("ERROR: package {s}: too many positional arguments.\n", .{package_name});
                    unreachable;
                },
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
                    .string, .identifier => |s| try self.putNewString(package_id, person_id, attr_id, s), // TODO: permissive

                    .function_call => |comment_fc| {
                        if (eql("c", comment_fc.name)) {
                            for (comment_fc.named) |comment_na| {
                                attr_id = try self.attributeId(comment_na.name);
                                switch (comment_na.value) {
                                    .string => |comment_s| {
                                        try self.putNewString(package_id, person_id, attr_id, comment_s);
                                    },
                                    .identifier => |comment_s| {
                                        // TODO: too permissive?
                                        try self.putNewString(package_id, person_id, attr_id, comment_s);
                                    },
                                    .function_call => |cfc| {
                                        if (eql("c", cfc.name)) {
                                            // comment = c(ORCID = c("123"))
                                            // put each positional as attribute value, and each named, ignoring name

                                            for (cfc.positional) |fa| switch (fa) {
                                                .string => |s| try self.putNewString(package_id, person_id, attr_id, s),
                                                else => {
                                                    std.debug.print("ERROR: package {s}: expected string in comment function argument.\n", .{package_name});
                                                    unreachable;
                                                },
                                            };
                                            for (cfc.named) |cna| switch (cna.value) {
                                                .string => |s| try self.putNewString(package_id, person_id, attr_id, s),
                                                else => {
                                                    std.debug.print("ERROR: package {s}: expected string in comment function named argument.\n", .{package_name});
                                                    unreachable;
                                                },
                                            };
                                        } else {
                                            std.debug.print("ERROR: package {s}: unsupported function in comment.\n", .{package_name});
                                            unreachable;
                                        }
                                    },
                                    .null => {},
                                }
                            }
                        }
                    },
                    .null => {},
                }
            } else if (eql("role", na.name)) {
                attr_id = try self.attributeId("role");
                switch (na.value) {
                    .string, .identifier => |s| switch (Role.fromString(s)) {
                        .unknown => {
                            try logUnknownRole(log, package_name, s);
                            try self.putExtraRole(package_id, person_id, s);
                        },
                        else => |code| try self.putNewRole(package_id, person_id, attr_id, code),
                    },

                    .function_call => |role_fc| {
                        if (eql("c", role_fc.name)) {
                            for (role_fc.positional) |fa| {
                                switch (fa) {
                                    .string, .identifier => |s| switch (Role.fromString(s)) {
                                        .unknown => {
                                            try logUnknownRole(log, package_name, s);
                                            try self.putExtraRole(package_id, person_id, s);
                                        },
                                        else => |code| try self.putNewRole(package_id, person_id, attr_id, code),
                                    },
                                    else => {
                                        try logExpectedStringInRole(log, package_name);
                                        continue;
                                    },
                                }
                            }
                            for (role_fc.named) |_| {
                                try logNamedArgumentInRole(log, package_name);
                                continue;
                            }
                        }
                    },
                    .null => {},
                }
            } else {
                attr_id = try self.attributeId(na.name);
                switch (na.value) {
                    .string => |s| try self.putNewString(package_id, person_id, attr_id, s),
                    .identifier => |s| {
                        // ignore named arguments with null value
                        if (eql("null", s)) continue;

                        // treat other identifier values as string
                        try self.putNewString(package_id, person_id, attr_id, s);
                    },
                    .function_call => |other_fc| {
                        // support vector of attribute values by appending index to attribute name
                        if (eql("c", other_fc.name)) {
                            for (other_fc.positional, 1..) |fa, pos| {
                                switch (fa) {
                                    .string, .identifier => |s| {
                                        // TODO: identifier as string too permissive?
                                        if (pos == 1) {
                                            // use unindexed name for first element
                                            try self.putNewString(package_id, person_id, attr_id, s);
                                        } else {
                                            // make an indexed name and use it
                                            var buf: [512]u8 = undefined;
                                            const indexed_name = try std.fmt.bufPrint(&buf, "{s}{}", .{ na.name, pos });
                                            const indexed_attr = try self.attributeId(try self.alloc.dupe(u8, indexed_name));
                                            try self.putNewString(package_id, person_id, indexed_attr, s);
                                        }
                                    },
                                    else => {
                                        std.debug.print("ERROR: non-string positional argument: {s}\n", .{package_name});
                                        return error.ParseError;
                                    },
                                }
                            }
                        }
                    },
                    .null => {},
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

    /// For roles that are not part of R's standard. See
    /// MARC_relator_db_codes_used_with_R and MARC_R_usage:
    /// https://github.com/r-devel/r-svn/blob/c20ebd2d417d9ebb915e32bfb0bfdad768f9a80a/src/library/utils/R/sysdata.R#L28C1-L39
    fn putExtraRole(
        self: *AuthorsDB,
        package_id: PackageId,
        person_id: PersonId,
        value: []const u8,
    ) !void {
        const S = struct {
            var extra_role: ?AttributeId = null; // static
        };
        if (S.extra_role == null) {
            S.extra_role = try self.attributeId("extra_role");
        }

        if (S.extra_role) |attr_id| {
            try self.person_strings.put(self.person_strings.data.nextId(), .{
                .package_id = package_id,
                .person_id = person_id,
                .attribute_id = attr_id,
                .value = value,
            });
        } else unreachable;
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
            for (self.data.items, 0..) |x, index| {
                if (std.meta.eql(x, value)) return @intCast(index);
            }
            return null;
        }
        pub fn lookupString(self: @This(), value: []const u8) ?IdType {
            for (self.data.items, 0..) |x, index| {
                if (std.mem.eql(u8, x, value)) return @intCast(index);
            }
            return null;
        }
    };
}

const AttributeId = u16;
const PersonAttributeId = u32;
const PersonId = u32;
const PackageId = u32;

pub const Role = enum(u8) {
    unknown, // must be first
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
    last, // must be last

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
            return .unknown;
        }
    }

    /// Undefined for .unknown and .last.
    pub fn toString(self: Role) []const u8 {
        return switch (self) {
            .author => "aut",
            .compiler => "com",
            .contributor => "ctb",
            .copyright_holder => "cph",
            .creator => "cre",
            .thesis_advisor => "ths",
            .translator => "trl",
            .contractor => "ctr",
            .data_contributor => "dtc",
            .funder => "fnd",
            .reviewer => "rev",
            .unknown, .last => unreachable,
        };
    }
};

/// Caller owns returned slice, which is allocated with Author's allocator.
pub fn read(self: *Authors, source: []const u8, strings: *StringStorage) ![]LogItem {
    const eql = std.ascii.eqlIgnoreCase;

    var log = LogItems.init(self.alloc);
    defer log.deinit();

    // arena for the RParser
    var arena = std.heap.ArenaAllocator.init(self.alloc);
    defer arena.deinit();
    const alloc = arena.allocator();

    // parse DCF
    var parser = Parser.init(arena.allocator(), strings);
    defer parser.deinit();
    try parser.parse(source);

    // iterate through parsed stanzas.

    const nodes = parser.nodes.items;
    var index: usize = 0;
    var package_name: ?[]const u8 = null;
    var prev_package_name: ?[]const u8 = null;
    var authors_source: ?[]const u8 = null;
    top: while (true) : (index += 1) switch (nodes[index]) {
        .eof => break,
        .stanza_end => {
            // parse authors field if any
            if (authors_source) |auth_source| {
                var rtokenizer = RTokenizer.init(auth_source, strings);
                defer rtokenizer.deinit();
                var rparser = RParser.init(alloc, &rtokenizer, strings);
                defer rparser.deinit();

                switch (try rparser.next()) {
                    .ok => |ok| switch (ok.node) {
                        .function_call => |fc| {
                            // outer function can be c() or person()
                            if (std.mem.eql(u8, "c", fc.name)) {
                                for (fc.positional) |fa| switch (fa) {
                                    .function_call => |c_fc| {
                                        if (eql("person", c_fc.name)) {
                                            try self.db.addFromFunctionCall(c_fc, package_name.?, &log);
                                        } else {
                                            try logNoPerson(&log, package_name.?);
                                            continue :top;
                                        }
                                    },
                                    else => {
                                        try logNoFunction(&log, package_name.?);
                                        continue :top;
                                    },
                                };
                            } else if (eql("person", fc.name)) {
                                try self.db.addFromFunctionCall(fc, package_name.?, &log);
                            } else {
                                try logNoFunction(&log, package_name.?);
                                continue :top;
                            }
                        },
                        .function_arg => {
                            try logNoFunction(&log, package_name.?);
                            // skip stanza and continue
                            while (true) switch (nodes[index]) {
                                .stanza_end, .eof => continue :top,
                                else => index += 1,
                            };
                        },
                    },
                    .err => |e| {
                        try logParseError(&log, e, package_name);
                        return error.RParseError;
                    },
                }
            } else if (package_name) |pname| {
                try logNoAuthorsAtR(&log, pname);
            } else {
                try logNoPackage(&log);
            }

            // reset package name and authors_source
            prev_package_name = package_name;
            package_name = null;
            authors_source = null;
        },
        .field => |field| {
            if (eql("package", field.name)) {
                package_name = try parsePackageName(nodes, &index, strings);
            } else if (eql("authors@r", field.name)) {
                // save authors@r field data to process at end of stanza.
                index += 1;
                switch (nodes[index]) {
                    .string_node => |s| authors_source = s.value,
                    else => {
                        try logAuthorsAtRWrongType(&log, package_name);
                        continue;
                    },
                }
            }
        },
        else => continue,
    };
    return try log.toOwnedSlice();
}

fn logParseError(log: *LogItems, e: RParser.ErrLoc, package_name: ?[]const u8) !void {
    if (package_name) |pname| {
        try log.append(.{ .tag = .{ .err = .rlang_parse_error }, .message = pname, .loc = 0, .extra = .{ .rlang_parse_err = e } });
    } else {
        try log.append(.{ .tag = .{ .err = .rlang_parse_error }, .message = "<unknown>", .loc = 0, .extra = .{ .rlang_parse_err = e } });
    }
}
fn logUnknownRole(log: *LogItems, package_name: []const u8, role: []const u8) !void {
    try log.append(.{ .tag = .{ .warn = .unknown_role_code }, .message = package_name, .loc = 0, .extra = .{ .string = role } });
}
fn logNoAuthorsAtR(log: *LogItems, package_name: []const u8) !void {
    try log.append(.{ .tag = .{ .info = .no_authors_at_r }, .message = package_name, .loc = 0 });
}
fn logNoFunction(log: *LogItems, package_name: []const u8) !void {
    try log.append(.{ .tag = .{ .warn = .no_function }, .message = package_name, .loc = 0 });
}
fn logNoPerson(log: *LogItems, package_name: []const u8) !void {
    try log.append(.{ .tag = .{ .warn = .no_person }, .message = package_name, .loc = 0 });
}
fn logNoPackage(log: *LogItems) !void {
    try log.append(.{ .tag = .{ .warn = .no_package }, .message = "", .loc = 0 });
}
fn logAuthorsAtRWrongType(log: *LogItems, package_name: ?[]const u8) !void {
    if (package_name) |pname| {
        try log.append(.{ .tag = .{ .err = .authors_at_r_wrong_type }, .message = pname, .loc = 0 });
    } else {
        try log.append(.{ .tag = .{ .err = .authors_at_r_wrong_type }, .message = "<unknown>", .loc = 0 });
    }
}
fn logExpectedStringInRole(log: *LogItems, package_name: []const u8) !void {
    try log.append(.{ .tag = .{ .err = .expected_string_in_role }, .message = package_name });
}
fn logNamedArgumentInRole(log: *LogItems, package_name: []const u8) !void {
    try log.append(.{ .tag = .{ .err = .named_argument_in_role }, .message = package_name });
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
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const source =
        \\Package: one
        \\Type: Package
        \\Title: Project Environments
        \\Version: 1.0.7.9000
        \\Authors@R: c(
        \\    person("Kevin", "Ushey", role = c("aut", "cre"), email = "kevin@rstudio.com",
        \\           comment = c(ORCID = "0000-0003-2880-7407")),
        \\    person("Hadley", "Wickham", role = c("aut"), email = "hadley@rstudio.com",
        \\           comment = c(ORCID = "0000-0003-4757-117X")),
        \\    person("Posit Software, PBC", role = c("cph", "fnd", "xyz", "zzz"))
        \\    )
        \\
        \\Package: one2
        \\Authors@R: person('Pierre-Yves', 'de Müllenheim', email = 'pydemull@uco.fr', role = c('cre', 'aut'), comment = c(ORCID = "0000-0001-9157-7371"))
        \\
        \\Package: two
        \\Authors@R: c(person(given = "Sy Han", family = "Chiou", email = "schiou@smu.edu", role = c("aut", "cre")),
        \\           person(given = "Sangwook", family = "Kang", role = "aut"),
        \\           person(given = "Jun", family = "Yan", role = "aut"))
        \\
        \\Package: three
        \\Authors@R: c(person(("Atanu"), "Bhattacharjee",
        \\                    email="atanustat@gmail.com",
        \\                    role=c("aut", "cre","ctb")),
        \\               person(("Gajendra Kumar"), "Vishwakarma", role=c("aut","ctb")),
        \\               person(("Pragya"), "Kumari", role=c("aut","ctb")))
        \\
        \\Package: four
        \\Authors@R: c(person("Patrick", "Mair", role = c("aut", "cre"), email = "mair@fas.harvard.edu"), person("Jan", "De Leeuw", role = "aut"))
        \\
        \\Package: five
        \\Authors@R: c(
        \\    person("Scott", "Chamberlain", role = "aut",
        \\        email = "myrmecocystus@gmail.com",
        \\        comment = c(ORCID="0000-0003-1444-9135")),
        \\    person("Hadley", "Wickham", role = "aut", email = "hadley@rstudio.com"),
        \\    person("Winston", "Chang", role = "aut", email = "winston@stdout.org"),
        \\    person("Bob", "Rudis", role = "ctb", email = "bob@rudis.net"),
        \\    person("Bryce", "Mecum", role = "ctb", email = "brycemecum@gmail.com",
        \\           comment = c("ORCID" = "0000-0002-0381-3766")),
        \\    person("Mauricio", "Vargas", role = c("aut", "cre"), email = "mavargas11@uc.cl",
        \\           comment = c(ORCID = "0000-0003-1017-7574")),
        \\    person("RStudio", role = "cph"),
        \\    person("DigitalOcean", role = "cph")
        \\    )
        \\
        \\Package: six
        \\Authors@R: c(person(given = c("Gavin", "L."), family = "Simpson",
        \\                    role = c("aut", "cre"),
        \\                    email = "ucfagls@gmail.com",
        \\                    comment = c(ORCID = "0000-0002-9084-8413"))
        \\           , person(given = "Jari", family = "Oksanen",  role = "aut")
        \\           , person(given = "Martin", family = "Maechler", role = "ctb")
        \\                    )
        \\
        \\Package: seven
        \\Authors@R: c(
        \\    person("Pierre", "Roudier", email = "roudierp@landcareresearch.co.nz", role = c("aut", "cre")),
        \\    person("Etienne", "Lalibert'{e}", email = NULL, role = c("ctb"))
        \\    )
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

    const log = try authors.read(source, &strings);

    for (log) |x| switch (x.tag) {
        .warn, .err => std.debug.print("{}\n", .{x}),
        .info => {},
    };
    authors.db.debugPrint();
}

test "read authors from PACKAGES-full.gz" {
    if (true) {
        const mos = @import("mos");

        var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
        defer arena.deinit();
        const alloc = arena.allocator();

        const path = "PACKAGES-full.gz";
        std.fs.cwd().access(path, .{}) catch return;

        const source: ?[]const u8 = try mos.file.readFileMaybeGzip(alloc, path);
        try std.testing.expect(source != null);
        defer if (source) |s| alloc.free(s);

        if (source) |source_| {
            var strings = try StringStorage.init(alloc, std.heap.page_allocator);
            defer strings.deinit();

            var authors = Authors.init(alloc);
            defer authors.deinit();

            var timer = try std.time.Timer.start();
            const log = try authors.read(source_, &strings);
            std.debug.print("Parse authors = {}ms\n", .{@divFloor(timer.lap(), 1_000_000)});

            authors.db.debugPrintInfo();

            for (log) |x| switch (x.tag) {
                .warn, .err => std.debug.print("{}\n", .{x}),
                .info => {},
            };
        }
    }
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
