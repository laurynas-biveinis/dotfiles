#!/bin/zsh
#
# Exported environment variables, CMake options:
# MY80xy, MY80xyD: Oracle MySQL release & debug build, for xy patchlevel version
# MARIA108, MARIA108D: MariaDB 10.8 release and debug build
# FB80xy, FB80xyD: MySQL 8.0 Facebook Patch release and debug
#
# Extra CMake options:
# MYCLANG, MYCLANG12, MYCLANG14, MYCLANG15, MYCLANG16, MYCLANG17: set compiler
# to clang for CMake, optionally with a version
# MYGCC13: set compiler to GCC 13 for CMake
# MY8SAN: add maximum supported Sanitizer configuration
#
# mysql-test-run options:
# MTR_EMD: MTR options to preload libeatmydata
#
# Shell functions:
# - mtr: call mtr with libeatmydata and some tmpdir
# - rmtr: rebuild and then call mtr
# - rm_tmp_mtr: cleanup all tmpdir from the above
# - mysql_cmake: in a build directory, figure out CMake options & run it
# - mysql_build: in a build directory, CMake and make
#
# Works on Linux and macOS (both Intel and Apple Silicon), XCode Command Line
# Tools 15.0.1.
# TODO(laurynas): var names that include version ranges should be consistent
# whether the lower or the higher version number goes first.

set -o errexit
set -o nounset
set -o pipefail

autoload mysql_find_version_file
autoload mysql_get_major_version
autoload mysql_get_minor_version
autoload mysql_need_openssl3_workaround
autoload mysql_maybe_workaround_openssl3
autoload mysql_undo_openssl3_workaround

mysql_export_environment_helpers() {
    declare -r uname_out="$(uname -s)"

    # Platform-specific stuff, both building blocks and complete user variables
    if [ "$uname_out" = "Darwin" ]; then
        declare -r brew="$(brew --prefix)/opt"
        if [ "$(arch)" = "arm64" ]; then
            # Workaround https://bugs.mysql.com/bug.php?id=113047 (MTR test
            # main.derived_limit fails with small cost differences) and
            # https://bugs.mysql.com/bug.php?id=113048 (MTR test
            # gis.gis_bugs_crashes fails with a result difference)
            declare -a -r my8_cxx_flags=("-ffp-contract=off")
            declare -a my8018_28=("-DWITH_SSL=$brew/openssl@1.1")
            export MY8030_820_CORE_DUMP_FLAGS=(
                "-DWITH_DEVELOPER_ENTITLEMENTS=ON")
        else
            declare -a -r my8_cxx_flags=()
            declare -a my8018_28=()
            export MY8030_820_CORE_DUMP_FLAGS=()
        fi
        declare -a my8018_extra=("-DWITH_ZSTD=bundled" "-DWITH_PROTOBUF=bundled")
        my8018_28+=("-DWITH_ICU=$brew/icu4c")
        declare -a -r my8018_28_cxx_flags=(
            "-Wno-shadow-field" "-Wno-unqualified-std-cast-call"
            "-Wno-deprecated-copy-with-dtor" "-Wno-bitwise-instead-of-logical"
            "-Wno-unused-label"
            # LLVM 14:
            "-Wno-error=unused-variable" "-Wno-error=missing-noreturn"
            "-Wno-error=extra-semi"
            # LLVM 14 RocksDB:
            "-Wno-error=invalid-offsetof"
            "-Wno-error=conditional-uninitialized"
            # LLVM 17:
            "-Wno-error=implicit-const-int-float-conversion")
        declare -a -r my8031_extra_cxx_flags=("-Wno-deprecated-declarations")
        declare -a -r my8032_34_extra_cxx_flags=(
            "-Wno-unused-but-set-variable" "-Wno-deprecated-copy-with-dtor")
        declare -a my8032_extra=("-DWITH_UNIT_TESTS=OFF")

        declare -a maria_common=(
            "-DCMAKE_C_FLAGS='-isystem /usr/local/include'"
            "-DCMAKE_CXX_FLAGS='-isystem /usr/local/include'")

        export MYCLANG12=("-DCMAKE_C_COMPILER=$brew/llvm@12/bin/clang"
                          "-DCMAKE_CXX_COMPILER=$brew/llvm@12/bin/clang++")
        # CMAKE_AR settings below workaround
        # https://bugs.mysql.com/bug.php?id=113113 (Build failure with Homebrew
        # LLVM 14-17 on macOS)
        export MYCLANG14=("-DCMAKE_C_COMPILER=$brew/llvm@14/bin/clang"
                          "-DCMAKE_CXX_COMPILER=$brew/llvm@14/bin/clang++"
                          "-DCMAKE_AR=$brew/llvm@14/bin/llvm-ar")
        export MYCLANG15=("-DCMAKE_C_COMPILER=$brew/llvm@15/bin/clang"
                          "-DCMAKE_CXX_COMPILER=$brew/llvm@15/bin/clang++"
                          "-DCMAKE_AR=$brew/llvm@15/bin/llvm-ar")
        export MYCLANG16=("-DCMAKE_C_COMPILER=$brew/llvm@16/bin/clang"
                          "-DCMAKE_CXX_COMPILER=$brew/llvm@16/bin/clang++"
                          "-DCMAKE_AR=$brew/llvm@16/bin/llvm-ar")
        export MYCLANG17=("-DCMAKE_C_COMPILER=$brew/llvm@17/bin/clang"
                          "-DCMAKE_CXX_COMPILER=$brew/llvm@17/bin/clang++"
                          "-DCMAKE_AR=$brew/llvm@17/bin/llvm-ar")
        export MYCLANG=("-DCMAKE_C_COMPILER=$brew/llvm/bin/clang"
                        "-DCMAKE_CXX_COMPILER=$brew/llvm/bin/clang++")
        export MYGCC13=("-DCMAKE_C_COMPILER=$brew/gcc/bin/gcc-13"
                        "-DCMAKE_CXX_COMPILER=$brew/gcc/bin/g++-13")

        declare -r emd_libdir="$brew/libeatmydata/lib/"
        export MTR_EMD=(
            "--mysqld-env=DYLD_LIBRARY_PATH=$emd_libdir"
            "--mysqld-env=DYLD_FORCE_FLAT_NAMESPACE=1"
            "--mysqld-env=DYLD_INSERT_LIBRARIES=$emd_libdir/libeatmydata.dylib")
    else
        declare -a my8018_extra=()
        declare -a my8018_28=()
        declare -a -r my8018_28_cxx_flags=("-Wno-unused-label")
        declare -a -r my8031_extra_cxx_flags=()
        declare -a -r my8032_34_extra_cxx_flags=()
        declare -a my8032_extra=()
        declare -a maria_common=()

        export MY8030_820_CORE_DUMP_FLAGS=()
        export MYCLANG12=("-DCMAKE_C_COMPILER=clang-12"
                          "-DCMAKE_CXX_COMPILER=clang++-12")
        export MYCLANG14=("-DCMAKE_C_COMPILER=clang-14"
                          "-DCMAKE_CXX_COMPILER=clang++-14")
        export MYCLANG15=("-DCMAKE_C_COMPILER=clang-15"
                          "-DCMAKE_CXX_COMPILER=clang++-15")
        export MYCLANG16=("-DCMAKE_C_COMPILER=clang-16"
                          "-DCMAKE_CXX_COMPILER=clang++-16")
        export MYCLANG17=("-DCMAKE_C_COMPILER=clang-17"
                          "-DCMAKE_CXX_COMPILER=clang++-17")
        export MYCLANG=("-DCMAKE_C_COMPILER=clang"
                        "-DCMAKE_CXX_COMPILER=clang++")
        export MYGCC13=("-DCMAKE_C_COMPILER=gcc-13"
                        "-DCMAKE_CXX_COMPILER=g++-13")

        export MTR_EMD=(
            "--mysqld-env=LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libeatmydata.so")
    fi

    # Platform-independent helpers

    export MYCLANG13=("-DCMAKE_C_COMPILER=clang-13"
                      "-DCMAKE_CXX_COMPILER=clang++-13")
    export MY8SAN=("-DWITH_ASAN=ON" "-DWITH_ASAN_SCOPE=ON" "-DWITH_UBSAN=ON")

    # Common building blocks

    declare -a -r cxx_flags_debug=("${my8_cxx_flags[@]}" "-g")
    declare -a -r cxx_flags_release=("${my8_cxx_flags[@]}" "-O2" "-g"
                                     "-DNDEBUG")

    declare -a -r cmake_common=("-DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
    declare -a -r cmake_release=("${cmake_common[@]}"
                                 "-DBUILD_CONFIG=mysql_release"
                                 "-DCMAKE_BUILD_TYPE=Release")
    declare -a -r cmake_debug=("${cmake_common[@]}" "-DCMAKE_BUILD_TYPE=Debug"
                               "-DWITH_DEBUG=ON")

    declare -a -r my8=("-DMYSQL_MAINTAINER_MODE=ON" "-DDOWNLOAD_BOOST=ON"
                       "-DWITH_BOOST=~/vilniusdb/mysql-boost/"
                       "-DWITH_SYSTEM_LIBS=ON")
    declare -a -r my8d=("${cmake_debug[@]}" "${my8[@]}")
    declare -a -r my8r=("${cmake_release[@]}" "${my8[@]}")

    # Workaround Facebook tooling incompatibility with git worktrees
    declare -a -r fb_common=(
        "-DMYSQL_GITHASH=0" "-DMYSQL_GITDATE=2100-02-29"
        "-DROCKSDB_GITHASH=0" "-DROCKSDB_GITDATE=2100-02-29")

    maria_common+=("-DPLUGIN_MROONGA=NO" "-DPLUGIN_CONNECT=NO")

    # Version-specific building blocks, descending order

    # 8.2.0--8.0.33

    declare -a -r my8033_820=("-DFORCE_COLORED_OUTPUT=ON")

    # 8.2.0

    declare -a -r my820_cxx_flags_debug=("${cxx_flags_debug[@]}")
    declare -a -r my820_cxx_flags_release=("${cxx_flags_release[@]}")
    declare -a -r my820_extra=(
        "-DCMAKE_CXX_FLAGS=${my8_cxx_flags[*]}"
        "-DCMAKE_C_FLAGS_DEBUG=${my820_cxx_flags_debug[*]}"
        "-DCMAKE_CXX_FLAGS_DEBUG=${my820_cxx_flags_debug[*]}"
        "-DCMAKE_CXX_FLAGS_RELEASE=${my820_cxx_flags_release[*]}")

    # 8.0.35

    declare -a -r my8035_cxx_flags_debug=("${cxx_flags_debug[@]}")
    declare -a -r my8035_cxx_flags_release=("${cxx_flags_release[@]}")
    declare -a -r my8035_extra=(
        "-DCMAKE_CXX_FLAGS=${my8_cxx_flags[*]}"
        "-DCMAKE_C_FLAGS_DEBUG=${my8035_cxx_flags_debug[*]}"
        "-DCMAKE_CXX_FLAGS_DEBUG=${my8035_cxx_flags_debug[*]}"
        "-DCMAKE_CXX_FLAGS_RELEASE=${my8035_cxx_flags_release[*]}")

    # 8.0.34

    declare -a -r my8034_cxx_flags_debug=("${my8032_34_extra_cxx_flags[@]}"
                                          "${cxx_flags_debug[@]}")
    declare -a -r my8034_cxx_flags_release=("${my8032_34_extra_cxx_flags[@]}"
                                            "${cxx_flags_release[@]}")
    declare -a -r my8034_extra=(
        "-DCMAKE_CXX_FLAGS=${my8032_34_extra_cxx_flags[*]}"
        "-DCMAKE_C_FLAGS_DEBUG=${my8034_cxx_flags_debug[*]}"
        "-DCMAKE_CXX_FLAGS_DEBUG=${my8034_cxx_flags_debug[*]}"
        "-DCMAKE_CXX_FLAGS_RELEASE=${my8034_cxx_flags_release[*]}")

    # 8.0.33

    declare -a -r my8033_cxx_flags_debug=("${my8032_34_extra_cxx_flags[@]}"
                                          "${cxx_flags_debug[@]}")
    declare -a -r my8033_cxx_flags_release=("${my8032_34_extra_cxx_flags[@]}"
                                            "${cxx_flags_release[@]}")
    declare -a -r my8033_extra=(
        "-DWITH_RAPIDJSON=bundled"
        "-DCMAKE_CXX_FLAGS=${my8032_34_extra_cxx_flags[*]}"
        "-DCMAKE_C_FLAGS_DEBUG=${my8033_cxx_flags_debug[*]}"
        "-DCMAKE_CXX_FLAGS_DEBUG=${my8033_cxx_flags_debug[*]}"
        "-DCMAKE_CXX_FLAGS_RELEASE=${my8033_cxx_flags_release[*]}")

    # 8.0.32

    declare -a -r my8032_cxx_flags_debug=("${my8032_34_extra_cxx_flags[@]}"
                                          "${cxx_flags_debug[@]}")
    declare -a -r my8032_cxx_flags_release=("${my8032_34_extra_cxx_flags[@]}"
                                            "${cxx_flags_release[@]}")
    my8032_extra+=("-DCMAKE_CXX_FLAGS=${my8032_34_extra_cxx_flags[*]}"
                   "-DCMAKE_C_FLAGS_DEBUG=${my8032_cxx_flags_debug[*]}"
                   "-DCMAKE_CXX_FLAGS_DEBUG=${my8032_cxx_flags_debug[*]}"
                   "-DCMAKE_CXX_FLAGS_RELEASE=${my8032_cxx_flags_release[*]}")

    # 8.0.31

    declare -a -r my8031_cxx_flags_debug=("${my8031_extra_cxx_flags[@]}"
                                          "${cxx_flags_debug[@]}")
    declare -a -r my8031_cxx_flags_release=("${my8031_extra_cxx_flags[@]}"
                                            "${cxx_flags_release[@]}")
    declare -a -r my8031_extra=(
        "-DCMAKE_CXX_FLAGS=${my8031_extra_cxx_flags[*]}"
        "-DCMAKE_C_FLAGS_DEBUG=${my8031_cxx_flags_debug[*]}"
        "-DCMAKE_CXX_FLAGS_DEBUG=${my8031_cxx_flags_debug[*]}"
        "-DCMAKE_CXX_FLAGS_RELEASE=${my8031_cxx_flags_release[*]}")

    # 8.0.29--8.0.28

    declare -a -r my8028_29_cxx_flags_debug=("-Wno-deprecated-declarations")

    # 8.0.29

    declare -a -r my8029_cxx_flags_debug=("${my8028_29_cxx_flags_debug[@]}"
                                          "${cxx_flags_debug[@]}")
    declare -a -r my8029_extra=(
        "-DCMAKE_C_FLAGS_DEBUG=${my8029_cxx_flags_debug[*]}"
        "-DCMAKE_CXX_FLAGS_DEBUG=${my8029_cxx_flags_debug[*]}"
        "-DCMAKE_CXX_FLAGS_RELEASE=${cxx_flags_release[*]}")

    # 8.0.28--8.0.27

    declare -a -r my8027_28=("-DWITH_FIDO=bundled")

    # 8.0.28--8.0.18

    declare -a -r my8018_28_cxx_flags_debug=("${my8018_28_cxx_flags[@]}"
                                             "-Wno-unknown-warning-option"
                                             "-Wno-unused-but-set-variable"
                                             "-Wno-discarded-qualifiers")

    my8018_28+=("-DWITH_RAPIDJSON=bundled" "-DWITH_LZ4=bundled")

    # 8.0.28

    declare -a -r fb8028_extra=("-DWITH_UNIT_TESTS=OFF")

    declare -a -r my8028_cxx_flags_debug=("${my8028_29_cxx_flags_debug[@]}"
                                          "${my8018_28_cxx_flags_debug[*]}"
                                          "${cxx_flags_debug[@]}")
    declare -a -r my8028_extra=(
        "-DCMAKE_CXX_FLAGS=${my8018_28_cxx_flags[*]}"
        "-DCMAKE_C_FLAGS_DEBUG=${my8028_cxx_flags_debug[*]}"
        "-DCMAKE_CXX_FLAGS_DEBUG=${my8028_cxx_flags_debug[*]}"
        "-DCMAKE_CXX_FLAGS_RELEASE=${cxx_flags_release[*]}")

    # 8.0.27

    declare -a -r my8027_cxx_flags_debug=("${my8018_28_cxx_flags[*]}"
                                          "${cxx_flags_debug[@]}")
    declare -a -r my8027_cxx_flags_release=("${my8018_28_cxx_flags[*]}"
                                            "${cxx_flags_release[@]}")
    declare -a -r my8027_extra=(
        "-DCMAKE_CXX_FLAGS=${my8018_28_cxx_flags[*]}"
        "-DCMAKE_C_FLAGS_DEBUG=${my8027_cxx_flags_debug[*]}"
        "-DCMAKE_CXX_FLAGS_DEBUG=${my8027_cxx_flags_debug[*]}"
        "-DCMAKE_CXX_FLAGS_RELEASE=${my8027_cxx_flags_release[*]}")

    # 8.0.26

    declare -a -r my8026_cxx_flags_debug=("${my8018_28_cxx_flags[*]}"
                                          "${my8018_28_cxx_flags_debug[*]}"
                                          "${cxx_flags_debug[@]}")
    declare -a -r my8026_cxx_flags_release=("${my8018_28_cxx_flags[*]}"
                                            "${cxx_flags_release[@]}")
    declare -a -r my8026_extra=(
        "-DENABLE_DOWNLOADS=ON"
        "-DCMAKE_CXX_FLAGS=${my8018_28_cxx_flags[*]}"
        "-DCMAKE_C_FLAGS_DEBUG=${my8026_cxx_flags_debug[*]}"
        "-DCMAKE_CXX_FLAGS_DEBUG=${my8026_cxx_flags_debug[*]}"
        "-DCMAKE_CXX_FLAGS_RELEASE=${my8026_cxx_flags_release[*]}")

    # 8.0.18

    declare -a -r my8018_extra_cxx_flags=("-Wno-deprecated-declarations"
                                          "-Wno-unused-result"
                                          "-Wno-range-loop-construct"
                                          "-Wno-non-c-typedef-for-linkage")

    declare -a -r my8018_cxx_flags=("${my8018_28_cxx_flags[*]}"
                                    "${my8018_extra_cxx_flags[*]}")
    declare -a -r my8018_cxx_flags_debug=("${my8018_28_cxx_flags[*]}"
                                          "${my8018_28_cxx_flags_debug[*]}"
                                          "${my8018_extra_cxx_flags[*]}"
                                          "${cxx_flags_debug[@]}")
    declare -a -r my8018_cxx_flags_release=("${my8018_28_cxx_flags[*]}"
                                            "${my8018_extra_cxx_flags[*]}"
                                            "${cxx_flags_release[@]}")
    my8018_extra+=(
        "-DCMAKE_CXX_FLAGS=${my8018_cxx_flags[*]}"
        "-DCMAKE_C_FLAGS_DEBUG=${my8018_cxx_flags_debug[*]}"
        "-DCMAKE_CXX_FLAGS_DEBUG=${my8018_cxx_flags_debug[*]}"
        "-DCMAKE_CXX_FLAGS_RELEASE=${my8018_cxx_flags_release[*]}")

    # Paydirt!

    export MY820D=("${my8d[@]}" "${my8033_820[@]}" "${my820_extra[@]}")
    export MY820=("${my8r[@]}" "${my8033_820[@]}" "${my820_extra[@]}")

    export MY810D=("${my8d[@]}" "${my8033_820[@]}")
    export MY810=("${my8r[@]}" "${my8033_820[@]}")

    export MY8035D=("${my8d[@]}" "${my8033_820[@]}" "${my8035_extra[@]}")
    export MY8035=("${my8r[@]}" "${my8033_820[@]}" "${my8035_extra[@]}")

    export MY8034D=("${my8d[@]}" "${my8033_820[@]}" "${my8034_extra[@]}")
    export MY8034=("${my8r[@]}" "${my8033_820[@]}" "${my8034_extra[@]}")

    export PS8034D=("${my8d[@]}" "${my8033_820[@]}" "${my8034_extra[@]}")

    export MY8033D=("${my8d[@]}" "${my8033_820[@]}" "${my8033_extra[@]}")
    export MY8033=("${my8r[@]}" "${my8033_820[@]}" "${my8033_extra[@]}")

    export MY8032D=("${my8d[@]}" "${my8032_extra[@]}")
    export MY8032=("${my8r[@]}" "${my8032_extra[@]}")

    export FB8032D=("${MY8032D[@]}" "${fb_common[@]}")
    export FB8032=("${MY8032[@]}" "${fb_common[@]}")

    export MY8031D=("${my8d[@]}" "${my8031_extra[@]}")

    export MY8030D=("${my8d[@]}")

    export MY8029D=("${my8d[@]}" "${my8029_extra[@]}")

    export MY8028=("${my8r[@]}" "${my8028_extra[@]}" "${my8018_28[@]}"
                   "${my8027_28[@]}")
    export MY8028D=("${my8d[@]}" "${my8028_extra[@]}" "${my8018_28[@]}"
                    "${my8027_28[@]}")

    export FB8028=("${MY8028[@]}" "${fb_common[@]}" "${fb8028_extra[@]}")
    export FB8028D=("${MY8028D[@]}" "${fb_common[@]}" "${fb8028_extra[@]}")

    export MY8027D=("${my8d[@]}" "${my8027_extra[@]}" "${my8018_28[@]}"
                    "${my8027_28[@]}")

    export MY8026=("${my8r[@]}" "${my8026_extra[@]}" "${my8018_28[@]}")
    export MY8026D=("${my8d[@]}" "-DDEBUG_EXTNAME=OFF" "${my8026_extra[@]}"
                    "${my8018_28[@]}")

    export MY8018=("${my8r[@]}" "${my8018_extra[@]}" "${my8018_28[@]}")
    export MY8018D=("${my8d[@]}" "${my8018_extra[@]}" "${my8018_28[@]}")

    export FB8026=("${MY8026[@]}" "${fb_common[@]}")
    export FB8026D=("${MY8026D[@]}" "${fb_common[@]}")

    export MARIA108=("${cmake_release[@]}" "${maria_common[@]}")
    export MARIA108D=("${cmake_debug[@]}" "${maria_common[@]}")
}

mysql_determine_flavor() {
    declare -r version_file="$1"

    if [ -d ./rocksdb ]; then
        echo "facebook"
        return 0
    fi

    declare -r version_extra_str=$(grep MYSQL_VERSION_EXTRA "$version_file")
    declare -r version_extra="${version_extra_str/MYSQL_VERSION_EXTRA=/}"
    if [ -n "$version_extra" ]; then
        echo "percona"
        return 0
    fi

    declare -r server_maturity=$(grep SERVER_MATURITY "$version_file")
    if [ -n "$server_maturity" ]; then
        echo "mariadb"
        return 0
    fi

    echo "mysql"
    return 0
}

mysql_cmake() {
    pushd .. || return 1

    if ! declare -r version_file=$(mysql_find_version_file); then
        popd
        return 1
    fi

    if ! declare -i -r major_ver=$(mysql_get_major_version "$version_file");
    then
        popd
        return 1
    fi

    if ! declare -i -r minor_ver=$(mysql_get_minor_version "$version_file");
    then
        popd
        return 1
    fi

    declare -r patch_level_str=$(grep MYSQL_VERSION_PATCH "$version_file")
    declare -i -r patch_level="${patch_level_str//[^0-9]/}"

    declare -r flavor=$(mysql_determine_flavor "$version_file")

    popd || return 1

    case "$flavor" in
        "mysql")
            echo "Configuring MySQL $major_ver.$minor_ver.$patch_level"
            case "$major_ver.$minor_ver.$patch_level" in
                8.2.0)
                    declare -a release_flags=("${MY820[@]}")
                    declare -a debug_flags=("${MY820D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_820_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.1.0)
                    declare -a release_flags=("${MY810[@]}")
                    declare -a debug_flags=("${MY810D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_820_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.35)
                    declare -a release_flags=("${MY8035[@]}")
                    declare -a debug_flags=("${MY8035D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_820_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.34)
                    declare -a release_flags=("${MY8034[@]}")
                    declare -a debug_flags=("${MY8034D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_820_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.33)
                    declare -a release_flags=("${MY8033[@]}")
                    declare -a debug_flags=("${MY8033D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_820_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.32)
                    declare -a release_flags=("${MY8032[@]}")
                    declare -a debug_flags=("${MY8032D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_820_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.31)
                    declare -a release_flags=("${MY8031[@]}")
                    declare -a debug_flags=("${MY8031D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_820_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.30)
                    declare -a release_flags=("${MY8030[@]}")
                    declare -a debug_flags=("${MY8030D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_820_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.29)
                    declare -a release_flags=("${MY8029[@]}")
                    declare -a debug_flags=("${MY8029D[@]}")
                    declare -a -r core_dump_flags=()
                    ;;
                8.0.28)
                    declare -a release_flags=("${MY8028[@]}")
                    declare -a debug_flags=("${MY8028D[@]}")
                    declare -a -r core_dump_flags=()
                    ;;
                8.0.18)
                    declare -a release_flags=("${MY8018[@]}")
                    declare -a debug_flags=("${MY8018D[@]}")
                    declare -a -r core_dump_flags=()
                    ;;
                *)
                    2>&1 echo "Unsupported version, please implement"
                    return 1
                    ;;
            esac
            ;;
        "facebook")
            echo "Configuring Facebook MySQL $major_ver.$minor_ver.$patch_level"
            case "$major_ver.$minor_ver.$patch_level" in
                8.0.32)
                    declare -a release_flags=("${FB8032[@]}")
                    declare -a debug_flags=("${FB8032D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_820_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.28)
                    declare -a release_flags=("${FB8028[@]}")
                    declare -a debug_flags=("${FB8028D[@]}")
                    declare -a -r core_dump_flags=()
                    ;;
                *)
                    2>&1 echo "Unsupported version, please add"
                    return 1
                    ;;
            esac
            ;;
        "percona")
            echo "Configuring Percona Server $major_ver.$minor_ver.$patch_level"
            case "$major_ver.$minor_ver.$patch_level" in
                8.0.34)
                    declare -a debug_flags=("${PS8034D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_820_CORE_DUMP_FLAGS[@]}")
                    ;;
                *)
                    2>&1 echo "Unsupported version, please add"
                    return 1
                    ;;
            esac
            ;;
        "mariadb")
            2>&1 echo "MariaDB unsupported, please add"
            return 1
            ;;
        *)
            2>&1 echo "Unsupported flavor, should have caught sooner"
            return 1
            ;;
    esac

    declare -r build_dir="$(basename "$PWD")"

    case "$build_dir" in
        *san*)
            echo "Using sanitizers"
            declare -i -r sanitizers=1
            debug_flags+=("${MY8SAN[@]}")
            release_flags+=("${MY8SAN[@]}")
            ;;
        *)
            echo "Not using sanitizers, enabling macOS core dumps if needed"
            declare -i -r sanitizers=0
            debug_flags+=("${core_dump_flags[@]}")
            release_flags+=("${core_dump_flags[@]}")
            ;;
    esac

    case "$build_dir" in
        *valgrind*)
            echo "Using Valgrind"
            if [ "$sanitizers" -eq 1 ]; then
                2>&1 echo "Valgrind is incompatible with sanitizers"
                return 1
            fi
            debug_flags+=("-DWITH_VALGRIND=ON")
            release_flags+=("-DWITH_VALGRIND=ON")
            ;;
    esac

    case "$build_dir" in
        *llvm-12*)
            echo "Using LLVM 12"
            debug_flags+=("${MYCLANG12[@]}")
            release_flags+=("${MYCLANG12[@]}")
            ;;
        *llvm-14*)
            echo "Using LLVM 14"
            debug_flags+=("${MYCLANG14[@]}")
            release_flags+=("${MYCLANG14[@]}")
            ;;
        *llvm-15*)
            echo "Using LLVM 15"
            debug_flags+=("${MYCLANG15[@]}")
            release_flags+=("${MYCLANG15[@]}")
            ;;
        *llvm-16*)
            echo "Using LLVM 16"
            debug_flags+=("${MYCLANG16[@]}")
            release_flags+=("${MYCLANG16[@]}")
            ;;
        *llvm-17*)
            echo "Using LLVM 17"
            debug_flags+=("${MYCLANG17[@]}")
            release_flags+=("${MYCLANG17[@]}")
            ;;
        *llvm*)
            echo "Using LLVM"
            debug_flags+=("${MYCLANG[@]}")
            release_flags+=("${MYCLANG[@]}")
            ;;
        *gcc-13*)
            echo "Using GCC 13"
            debug_flags+=("${MYGCC13[@]}")
            release_flags+=("${MYGCC13[@]}")
            ;;
    esac

    case "$build_dir" in
        *debug*)
            echo "Debug build"
            cmake -G Ninja .. "${debug_flags[@]}" "$@" || return 1
            ;;
        *release*)
            echo "Release build"
            cmake -G Ninja .. "${release_flags[@]}" "$@" || return 1
            ;;
    esac
}

mysql_build() {
    git submodule update --init --recursive
    mysql_cmake "$@" || return 1

    declare -r build_dir="$(basename "$PWD")"
    pushd .. || return 1

    ln -sf "$build_dir/compile_commands.json" .

    if ! declare -i workaround_ssl3=$(mysql_need_openssl3_workaround); then
        popd
        return 1
    fi

    popd || return 1

    mysql_maybe_workaround_openssl3 $workaround_ssl3
    ninja
    mysql_undo_openssl3_workaround $workaround_ssl3
}

mtr() {
    declare -r mktemp_template="/tmp/mtr-XXXX"
    declare -r mtr_emd_tmp_dir=$(mktemp -d $mktemp_template) ||
        { 2>&1 echo "Failed to create a temp dir like $mktemp_template";
          return 1; }

    ./mtr "${MTR_EMD[@]}" --tmpdir="$mtr_emd_tmp_dir" "$@"
}

rmtr() {
    ninja -C .. || return 1
    mtr "$@"
}

rm_tmp_mtr() {
    rm -rf /tmp/mtr-* /tmp/router-* /tmp/mysqld-* /tmp/mysql-unique-ids
}

mysql_export_environment_helpers

# Workaround P10K going crazy
set +o errexit
set +o nounset
