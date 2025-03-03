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

const Build = std.Build;
const Compile = std.Build.Step.Compile;
const Module = std.Build.Module;
const ResolvedTarget = Build.ResolvedTarget;
const OptimizeMode = std.builtin.OptimizeMode;

pub fn build(b: *Build) !void {
    // -- begin options ------------------------------------------------------

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // -- end options --------------------------------------------------------

    // -- begin dependencies -------------------------------------------------

    const mos = b.dependency("mos", .{
        .target = target,
        .optimize = optimize,
    }).module("mos");

    const dcf = b.dependency("dcf", .{
        .target = target,
        .optimize = optimize,
    }).module("dcf");

    const rlang = b.dependency("rlang", .{
        .target = target,
        .optimize = optimize,
    }).module("rlang");

    const cmdline = b.lazyDependency("cmdline", .{
        .target = target,
        .optimize = optimize,
    });
    const mosql = b.lazyDependency("mosql", .{
        .target = target,
        .optimize = optimize,
    });

    // -- end dependencies -----------------------------------------------------

    // -- begin module ---------------------------------------------------------

    const mod = b.addModule("r_repo_parse", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    mod.addImport("mos", mos);
    mod.addImport("dcf", dcf);
    mod.addImport("rlang", rlang);

    // -- end module ----------------------------------------------------------

    // -- begin executable ----------------------------------------------------

    const exe = b.addExecutable(.{
        .name = "parse-authors",
        .root_source_file = b.path("src/exe/parse-authors.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("mos", mos);
    exe.root_module.addImport("r_repo_parse", mod);

    if (cmdline) |dep| {
        exe.root_module.addImport("cmdline", dep.module("cmdline"));
    }
    if (mosql) |dep| {
        exe.root_module.addImport("mosql", dep.module("mosql"));
        exe.linkLibC();
    }
    b.installArtifact(exe);

    // -- end executable ------------------------------------------------------

    // -- begin test ----------------------------------------------------------

    const test_filters: []const []const u8 = b.option(
        []const []const u8,
        "test_filter",
        "Skip tests that do not match any of the specified filters",
    ) orelse &.{};

    const lib_unit_tests = b.addTest(.{
        .root_module = mod,
        .filters = test_filters,
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);

    // -- end test ------------------------------------------------------------

    // -- begin check ---------------------------------------------------------

    const check_exe = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    check_exe.root_module.addImport("mos", mos);

    const check = b.step("check", "Check if root compiles.");
    check.dependOn(&check_exe.step);

    // -- end check -----------------------------------------------------------
}
