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
const mos = @import("mos");

// Re-export this dependency because some of its types are used in
// public APIs of this library, and others are useful to other
// consumers (for example, the r-build-zig project).
pub const rlang = @import("rlang");

pub const repository = @import("repository_tools.zig");
pub const Authors = @import("Authors.zig");

pub const Repository = repository.Repository;

test {
    _ = repository;
    _ = Authors;
}
