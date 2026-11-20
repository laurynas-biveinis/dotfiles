"""Exercise mysql_cmake with real version files and a recording CMake stub."""

import shlex
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path
from typing import NamedTuple

from mysql_work_test_utils import run_sourced_zsh

# The markers mysql_determine_flavor detects: a rocksdb/ directory and a
# MYSQL_VERSION_EXTRA line.  An unlisted flavor raises KeyError.
FLAVOR_TREES = {
    "mysql": (False, ""),
    "facebook": (True, ""),
    "percona": (False, "MYSQL_VERSION_EXTRA=-28\n"),
}


class Build(NamedTuple):
    """A build directory to configure: its name, source tree and commands."""

    name: str
    version: str = "8.0.46"
    flavor: str = "mysql"
    commands: str = "mysql_cmake"


class MysqlCmakeTest(unittest.TestCase):
    """Check the options passed to CMake without configuring a real server."""

    def setUp(self):
        self.root = Path(tempfile.mkdtemp(prefix="mysql cmake "))
        self.addCleanup(shutil.rmtree, self.root)

    def run_sourced(self, ostype, commands, *arguments):
        """Run COMMANDS with ARGUMENTS in zsh after sourcing SCRIPT for OSTYPE."""
        result = run_sourced_zsh(
            r"""
set -e
OSTYPE=$2
fpath=("${1:h:h}/functions" $fpath)
source "$1"
shift 2
""" + commands,
            "mysql-cmake-test",
            ostype,
            *arguments,
        )
        self.assertEqual((result.returncode, result.stderr), (0, ""))
        return result.stdout

    def configure(self, builds, ostype="darwin"):
        """Source the configuration once and run each build's commands.

        Each build runs in its own subshell, so it starts from the sourced state
        as a fresh shell would.  Its outputs land in its source directory.
        """
        arguments = []
        for build in builds:
            major, minor, patch = build.version.split(".")
            source_dir = self.root / build.name
            source_dir.mkdir()
            has_rocksdb, extra = FLAVOR_TREES[build.flavor]
            if has_rocksdb:
                (source_dir / "rocksdb").mkdir()
            (source_dir / "MYSQL_VERSION").write_text(
                f"MYSQL_VERSION_MAJOR={major}\nMYSQL_VERSION_MINOR={minor}\n"
                f"MYSQL_VERSION_PATCH={patch}\n{extra}",
                encoding="utf-8",
            )
            build_dir = source_dir / build.name
            build_dir.mkdir()
            arguments += [str(build_dir), build.commands]
        stdout = self.run_sourced(
            ostype,
            r"""
set +e
cmake() { printf '%s\0' "$@" > "$test_cmake_args"; }
while (( $# )); do
	test_build_dir=$1 test_commands=$2
	shift 2
	(
		set -e
		cd -- "$test_build_dir"
		test_cmake_args=${test_build_dir:h}/cmake-args
		eval "$test_commands"
	) > "${test_build_dir:h}/stdout" 2> "${test_build_dir:h}/stderr"
	print $? > "${test_build_dir:h}/status"
done
""",
            *arguments,
        )
        self.assertEqual(stdout, "")
        return {
            build.name: subprocess.CompletedProcess(
                build.name,
                int(self.read_output(build.name, "status")),
                self.read_output(build.name, "stdout"),
                self.read_output(build.name, "stderr"),
            )
            for build in builds
        }

    def read_output(self, build_name, output):
        """Read one output that configure saved for BUILD_NAME."""
        return (self.root / build_name / output).read_text(encoding="utf-8")

    def cmake_called(self, build_name):
        """Whether the CMake stub ran for BUILD_NAME, which configure must have run."""
        self.assertTrue((self.root / build_name / "status").is_file(), build_name)
        return (self.root / build_name / "cmake-args").exists()

    def cmake_arguments(self, build_name):
        """Read the stub's NUL-delimited arguments; a missing call is an error."""
        return self.read_output(build_name, "cmake-args").split("\0")[:-1]

    def compiler_presets(self, ostype):
        """List the compiler preset names that sourcing defines for OSTYPE."""
        return self.run_sourced(
            ostype, "print -l ${(ok)parameters[(I)MY(CLANG|GCC)*]}"
        ).split()

    def test_modifiers_apply_to_every_build_type(self):
        """Base options, modifiers and explicit overrides arrive in that order."""
        core_dump = ["-DWITH_DEVELOPER_ENTITLEMENTS=ON"]
        cases = (
            (
                "san-llvm-18",
                ["-DWITH_ASAN=ON", "-DWITH_ASAN_SCOPE=ON", "-DWITH_UBSAN=ON"],
                [
                    "-DCMAKE_C_COMPILER=/stub-brew/opt/llvm@18/bin/clang",
                    "-DCMAKE_CXX_COMPILER=/stub-brew/opt/llvm@18/bin/clang++",
                    "-DCMAKE_AR=/stub-brew/opt/llvm@18/bin/llvm-ar",
                ],
            ),
            (
                "valgrind-gcc-14",
                core_dump + ["-DWITH_VALGRIND=ON"],
                [
                    "-DCMAKE_C_COMPILER=/stub-brew/bin/gcc-14",
                    "-DCMAKE_CXX_COMPILER=/stub-brew/bin/g++-14",
                ],
            ),
            (
                "llvm",
                core_dump,
                [
                    "-DCMAKE_C_COMPILER=/stub-brew/opt/llvm/bin/clang",
                    "-DCMAKE_CXX_COMPILER=/stub-brew/opt/llvm/bin/clang++",
                    "-DCMAKE_AR=/stub-brew/opt/llvm/bin/llvm-ar",
                ],
            ),
        )
        overrides = [
            "-DCMAKE_CXX_FLAGS=custom with spaces",
            "-DCMAKE_BUILD_TYPE=Custom",
        ]
        builds = [
            (
                Build(
                    f"_build-{build_type}-{modifier}",
                    commands=shlex.join(["mysql_cmake", *overrides]),
                ),
                build_type,
                modifier,
                instrumentation,
                compiler,
            )
            for build_type in ("debug", "release", "benchmark")
            for modifier, instrumentation, compiler in cases
        ]
        results = self.configure([build for build, *_ in builds])
        for build, build_type, modifier, instrumentation, compiler in builds:
            with self.subTest(build_type=build_type, modifier=modifier):
                result = results[build.name]
                self.assertEqual((result.returncode, result.stderr), (0, ""))
                arguments = self.cmake_arguments(build.name)
                self.assertEqual(arguments[:3], ["-G", "Ninja", ".."])
                expected_type = "Debug" if build_type == "debug" else "Release"
                self.assertIn(f"-DCMAKE_BUILD_TYPE={expected_type}", arguments)
                self.assertEqual(
                    arguments.count("-DWITH_UNIT_TESTS=OFF"),
                    1 if build_type == "benchmark" else 0,
                )
                suffix = instrumentation + compiler + overrides
                self.assertEqual(arguments[-len(suffix) :], suffix)
                self.assertEqual(arguments.count("-G"), 1)
                self.assertEqual(
                    arguments.count("-DWITH_DEVELOPER_ENTITLEMENTS=ON"),
                    0 if modifier.startswith("san") else 1,
                )
                self.assertIn(
                    f"CMake options: {' '.join(arguments[3:])}\n", result.stdout
                )

    def test_compiler_presets_exist_on_every_platform(self):
        """A preset defined for one platform only leaves the other unselected."""
        darwin = self.compiler_presets("darwin")
        self.assertIn("MYCLANG20", darwin)
        self.assertEqual(darwin, self.compiler_presets("linux-gnu"))

    def test_linux_compiler_modifiers_select_their_compiler(self):
        """Each Linux compiler modifier selects both its C and C++ compiler."""
        compilers = [("llvm", "clang", "clang++")]
        compilers += [
            (f"llvm-{v}", f"clang-{v}", f"clang++-{v}") for v in (12, *range(14, 21))
        ]
        compilers += [(f"gcc-{v}", f"gcc-{v}", f"g++-{v}") for v in range(11, 15)]
        results = self.configure(
            [Build(f"_build-debug-{modifier}") for modifier, _, _ in compilers],
            ostype="linux-gnu",
        )
        for modifier, c_compiler, cxx_compiler in compilers:
            with self.subTest(modifier=modifier):
                build_name = f"_build-debug-{modifier}"
                result = results[build_name]
                self.assertEqual((result.returncode, result.stderr), (0, ""))
                self.assertEqual(
                    [
                        argument
                        for argument in self.cmake_arguments(build_name)
                        if argument.startswith(
                            ("-DCMAKE_C_COMPILER=", "-DCMAKE_CXX_COMPILER=")
                        )
                    ],
                    [
                        f"-DCMAKE_C_COMPILER={c_compiler}",
                        f"-DCMAKE_CXX_COMPILER={cxx_compiler}",
                    ],
                )

    def test_unsupported_build_types_are_rejected(self):
        """Neither modifiers nor caller globals may stand in for base options."""
        cases = (
            ("benchmark", "8.0.44", "mysql"),
            ("release", "8.0.29", "mysql"),
            ("release", "8.0.30", "mysql"),
            ("release", "8.0.31", "mysql"),
            ("release", "8.0.34", "percona"),
            ("release", "8.0.35", "percona"),
            ("release", "8.0.36", "percona"),
            ("benchmark", "8.0.36", "facebook"),
        )
        builds = [
            (
                Build(
                    f"_build-{build_type}-san-llvm-18-{flavor}-{version}",
                    version=version,
                    flavor=flavor,
                    commands=f"{build_type}_flags=(-DLEAKED=ON); mysql_cmake",
                ),
                build_type,
            )
            for build_type, version, flavor in cases
        ]
        results = self.configure([build for build, _ in builds])
        for build, build_type in builds:
            with self.subTest(
                build_type=build_type, version=build.version, flavor=build.flavor
            ):
                result = results[build.name]
                self.assertEqual(
                    (result.returncode, result.stderr),
                    (
                        1,
                        f"{build_type} build unsupported for {build.flavor} "
                        f"{build.version}\n",
                    ),
                )
                self.assertFalse(self.cmake_called(build.name))

    def test_undefined_base_options_are_rejected(self):
        """Zsh expands an undefined preset to one empty argument, not to none."""
        build = Build("_build-release", commands="unset MY8046; mysql_cmake")
        result = self.configure([build])[build.name]
        self.assertEqual(
            (result.returncode, result.stderr),
            (1, "release build unsupported for mysql 8.0.46\n"),
        )
        self.assertFalse(self.cmake_called(build.name))

    def test_invalid_build_dir_names_are_rejected(self):
        """A name must pick a build type and not combine sanitizers with Valgrind."""
        cases = {
            "_build-llvm": "Must choose one of debug, release, benchmark\n",
            "_build-debug-san-valgrind": "Valgrind is incompatible with sanitizers\n",
        }
        results = self.configure([Build(name) for name in cases])
        for name, message in cases.items():
            with self.subTest(name=name):
                result = results[name]
                self.assertEqual((result.returncode, result.stderr), (1, message))
                self.assertFalse(self.cmake_called(name))


if __name__ == "__main__":
    unittest.main()
