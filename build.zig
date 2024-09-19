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

const targets: []const std.Target.Query = &.{
    .{ .cpu_arch = .aarch64, .os_tag = .macos },
    .{ .cpu_arch = .aarch64, .os_tag = .linux },
    .{ .cpu_arch = .x86_64, .os_tag = .macos },
    .{ .cpu_arch = .x86_64, .os_tag = .linux },
    .{ .cpu_arch = .x86_64, .os_tag = .windows },
};

pub fn build(b: *Build) !void {
    // -- begin options ------------------------------------------------------
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    // -- end options --------------------------------------------------------

    // -- begin dependencies -------------------------------------------------

    const common = b.dependency("common", .{
        .target = target,
        .optimize = optimize,
    }).module("common");

    const mos = b.dependency("mos", .{
        .target = target,
        .optimize = optimize,
    }).module("mos");

    // -- end dependencies -----------------------------------------------------

    // -- begin C static library -----------------------------------------------

    var lib_for_docs: ?*std.Build.Step.Compile = null;

    for (targets) |t| {
        const target_ = b.resolveTargetQuery(t);
        const lib = b.addStaticLibrary(.{
            .name = "rrepoparse",
            .root_source_file = b.path("src/c.zig"),
            .target = target_,
            .optimize = optimize,

            // position independent code
            .pic = true,

            // this prevents the inclusion of stack trace printing
            // code, which is roughly 500k.
            // https://ziggit.dev/t/strip-option-in-build-zig/1371/8
            .strip = true,
        });
        lib.root_module.addImport("common", common);
        lib.linkLibC();

        // just take the first one, it doesn't matter which
        if (lib_for_docs == null) lib_for_docs = lib;

        const target_out = b.addInstallArtifact(lib, .{
            .dest_dir = .{
                .override = .{
                    .custom = try t.zigTriple(b.allocator),
                },
            },
        });

        b.getInstallStep().dependOn(&target_out.step);
    }

    // -- end C static library -----------------------------------------------

    // -- begin module -------------------------------------------------------

    const mod = b.addModule("r-repo-parse", .{
        .root_source_file = b.path("src/repository.zig"),
        .target = target,
        .optimize = optimize,
    });
    mod.addImport("mos", mos);
    mod.addImport("common", common);

    // -- end module ---------------------------------------------------------

    // -- begin test ---------------------------------------------------------

    const lib_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    lib_unit_tests.root_module.addImport("common", common);
    lib_unit_tests.root_module.addImport("mos", mos);
    lib_unit_tests.linkLibC();

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);

    // -- end test -----------------------------------------------------------

    // -- begin generated documentation ------------------------------------

    if (lib_for_docs) |lib| {
        const doc_step = b.step("doc", "Generate documentation");
        const doc_install = b.addInstallDirectory(.{
            .install_dir = .prefix,
            .install_subdir = "doc",
            .source_dir = lib.getEmittedDocs(),
        });
        doc_install.step.dependOn(&lib.step);
        doc_step.dependOn(&doc_install.step);
        b.getInstallStep().dependOn(&doc_install.step);
    }

    // -- end generated documentation --------------------------------------
}
