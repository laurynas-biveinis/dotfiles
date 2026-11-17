"""Exercise mysql_cmake with real version files and a recording CMake stub."""

import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

from mysql_work_test_utils import SCRIPT, SOURCE_STUBS


class MysqlCmakeTest(unittest.TestCase):
    """Check the options passed to CMake without configuring a real server."""

    def setUp(self):
        self.root = Path(tempfile.mkdtemp(prefix="mysql cmake "))
        self.addCleanup(shutil.rmtree, self.root)

    def run_sourced(self, ostype, commands, *arguments):
        """Run COMMANDS with ARGUMENTS in zsh after sourcing SCRIPT for OSTYPE."""
        result = subprocess.run(
            [
                "/bin/zsh",
                "-f",
                "-c",
                SOURCE_STUBS + r"""
set -e
OSTYPE=$1
fpath=("${2:h:h}/functions" $fpath)
source "$2"
shift 2
""" + commands,
                "mysql-cmake-test",
                ostype,
                str(SCRIPT),
                *arguments,
            ],
            check=False,
            capture_output=True,
            text=True,
        )
        self.assertEqual((result.returncode, result.stderr), (0, ""))
        return result.stdout

    def configure(self, build_names, ostype):
        """Source the configuration once and run mysql_cmake in each build dir.

        Each build runs in its own subshell, so it starts from the sourced state
        as a fresh shell would.  Its outputs land in its source directory.
        """
        build_dirs = []
        for build_name in build_names:
            source_dir = self.root / build_name
            source_dir.mkdir()
            (source_dir / "MYSQL_VERSION").write_text(
                "MYSQL_VERSION_MAJOR=8\nMYSQL_VERSION_MINOR=0\nMYSQL_VERSION_PATCH=46\n",
                encoding="utf-8",
            )
            build_dir = source_dir / build_name
            build_dir.mkdir()
            build_dirs.append(str(build_dir))
        stdout = self.run_sourced(
            ostype,
            r"""
set +e
cmake() { printf '%s\0' "$@" > "$test_cmake_args"; }
for test_build_dir in "$@"; do
	(
		set -e
		cd -- "$test_build_dir"
		test_cmake_args=${test_build_dir:h}/cmake-args
		mysql_cmake
	) > "${test_build_dir:h}/stdout" 2> "${test_build_dir:h}/stderr"
	print $? > "${test_build_dir:h}/status"
done
""",
            *build_dirs,
        )
        self.assertEqual(stdout, "")
        return {
            build_name: subprocess.CompletedProcess(
                build_name,
                int(self.read_output(build_name, "status")),
                self.read_output(build_name, "stdout"),
                self.read_output(build_name, "stderr"),
            )
            for build_name in build_names
        }

    def read_output(self, build_name, output):
        """Read one output that configure saved for BUILD_NAME."""
        return (self.root / build_name / output).read_text(encoding="utf-8")

    def cmake_arguments(self, build_name):
        """Read the stub's NUL-delimited arguments; a missing call is an error."""
        return self.read_output(build_name, "cmake-args").split("\0")[:-1]

    def compiler_presets(self, ostype):
        """List the compiler preset names that sourcing defines for OSTYPE."""
        return self.run_sourced(
            ostype, "print -l ${(ok)parameters[(I)MY(CLANG|GCC)*]}"
        ).split()

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
            [f"_build-debug-{modifier}" for modifier, _, _ in compilers],
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


if __name__ == "__main__":
    unittest.main()
