#!/bin/zsh
#
# Exported environment variables, CMake options:
# MYxyz, MYxyzD: Oracle MySQL release & debug build, for x.y.z version
# MARIA108, MARIA108D: MariaDB 10.8 release and debug build
# FB80xy, FB80xyD: MySQL 8.0 Facebook Patch release and debug
#
# Extra CMake options:
# MYCLANG, MYCLANG12, MYCLANG14, MYCLANG15, MYCLANG16, MYCLANG17: set compiler
# to clang for CMake, optionally with a version
# MYGCC13: set compiler to GCC 13 for CMake
# MYSAN: add maximum supported Sanitizer configuration
#
# mysql-test-run options:
# MTR_EMD: MTR options to preload libeatmydata
#
# User shell functions:
# - mtr: call mtr with libeatmydata and some tmpdir
# - rmtr: rebuild and then call mtr
# - rm_tmp_mtr: cleanup all tmpdir from the above
# - mysql_cmake: in a build directory, figure out CMake options & run it. Any
#   arguments will be passed through to CMake
# - mysql_build: in a build directory, CMake and build
# - mysql_build_all: build all build dirs below
# - mysql_rebuild: in a build directory, build
# - mysql_rebuild_all: rebuild all build dirs below
#
# Works on Linux and macOS (both Intel and Apple Silicon), XCode Command Line
# Tools 16.0.

set -o errexit
set -o nounset
set -o pipefail

# zsh functions
autoload is-at-least

# mysql-work functions
autoload mysql_find_version_file
autoload mysql_get_major_version
autoload mysql_get_minor_version
autoload mysql_get_patch_level
autoload mysql_need_openssl3_workaround
autoload mysql_maybe_workaround_openssl3
autoload mysql_undo_openssl3_workaround

mysql_export_build_defaults() {
    # The default build type for mysql_build if not in a build dir and no build
    # type was passed
    export MY_DEFAULT_BUILD_TYPE="debug"
    # The prefix for the build directories
    export MY_BUILD_DIR_PREFIX="_build-"
}

# Enable when developing
mysql_validate_version() {
    declare -r version="$1"
    if ! [[ $version =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
        2>&1 echo "Invalid version $version. Expected format X.Y.Z"
        return 1
    fi
    echo "$version"
    return 0
}

mysql_version_compare() {
    if ! declare -r v1=$(mysql_validate_version "$1"); then
        return 1
    fi
    if ! declare -r v2=$(mysql_validate_version "$1"); then
        return 1
    fi

    if is-at-least "$1" "$2"; then
        if [[ "$1" == "$2" ]]; then
            echo 0
        else
            echo -1
        fi
    else
        echo 1
    fi
}

# Enable when developing
mysql_validate_compiler_flag_type() {
    declare -r type="$1"
    if ! [[ $type =~ ^(cxx|cxx_debug|cxx_release)$ ]]; then
        2>&1 echo \
             "Invalid flag type $type. Can be one of cxx, cxx_debug, cxx_release"
        return 1
    fi
    echo "$type"
    return 0
}

mysql_add_comp_flags() {
    if ! declare -r start_version=$(mysql_validate_version "$1"); then
        return 1
    fi
    if ! declare -r end_version=$(mysql_validate_version "$2"); then
        return 1
    fi
    if [[ $(mysql_version_compare "$start_version" "$end_version") -gt 0 ]];
    then
        2>&1 echo "Start $start_version cannot be higher than end $end_version"
    fi
    if ! declare -r type=$(mysql_validate_compiler_flag_type "$3"); then
        return 1
    fi
    shift 3
    declare -r flags=("$@")
    declare -r key="${start_version}-${end_version}:${type}"
    # Enable when debugging
    if [[ -n "${comp_flags[$key]:-}" ]]; then
        2>&1 echo "Attempt to overwrite flags for $key"
    fi
    comp_flags[$key]="${flags[*]}"
    return 0
}

mysql_get_comp_flags() {
    if ! declare -r version=$(mysql_validate_version "$1"); then
        return 1
    fi
    if ! declare -r type=$(mysql_validate_compiler_flag_type "$2"); then
        return 1
    fi
    declare -a flags=()
    for range in "${(k)comp_flags[@]}"; do
        IFS='-:' read -r start_version end_version range_flag_type <<< "$range"
        if [[ $(mysql_version_compare "$version" "$start_version") -ge 0 ]] &&
               [[ $(mysql_version_compare "$version" "$end_version") -le 0 ]] &&
               [[ "$type" == *"$range_flag_type"* ]]; then
            flags+=("${comp_flags[$range]}")
        fi
    done
    echo ${flags[*]}
    return 0
}

# Enable when developing
mysql_validate_cmake_flag_type() {
    declare -r type="$1"
    if ! [[ $type =~ ^(any|any_debug|any_release)$ ]]; then
        2>&1 echo \
             "Invalid flag type $type. Can be one of any, any_debug, any_release"
        return 1
    fi
    echo "$type"
    return 0
}

mysql_add_cmake_flags() {
    if ! declare -r start_version=$(mysql_validate_version "$1"); then
        return 1
    fi
    if ! declare -r end_version=$(mysql_validate_version "$2"); then
        return 1
    fi
    if [[ $(mysql_version_compare "$start_version" "$end_version") -gt 0 ]];
    then
        2>&1 echo "Start $start_version cannot be higher than end $end_version"
    fi
    if ! declare -r type=$(mysql_validate_cmake_flag_type "$3"); then
        return 1
    fi
    shift 3
    declare -r flags=("$@")
    declare -r key="${start_version}-${end_version}:${type}"
    if [[ ! -v cmake_flags[$key] ]]; then
        cmake_flags[$key]=""
    fi
    cmake_flags[$key]+=" ${flags[@]}"
    return 0
}

mysql_get_cmake_flags() {
    if ! declare -r version=$(mysql_validate_version "$1"); then
        return 1
    fi
    if ! declare -r type=$(mysql_validate_compiler_flag_type "$2"); then
        return 1
    fi
    declare -a flags=()
    for range in "${(k)cmake_flags[@]}"; do
        IFS='-:' read -r start_version end_version range_flag_type <<< "$range"
        if [[ $(mysql_version_compare "$version" "$start_version") -ge 0 ]] &&
               [[ $(mysql_version_compare "$version" "$end_version") -le 0 ]] &&
               [[ "$type" == *"$range_flag_type"* ]]; then
            flags+=("${cmake_flags[$range]}")
        fi
    done
    echo "${flags[@]}"
    return 0
}

mysql_export_environment_helpers() {
    declare -r uname_out="$(uname -s)"

    declare -gA comp_flags=()
    declare -gA cmake_flags=()

    declare -r min_version="8.0.18"
    declare -r max_version="9.2.0"

    # Platform-specific stuff, both building blocks and complete user variables
    if [ "$uname_out" = "Darwin" ]; then
        declare -r brew="$(brew --prefix)"
        declare -r brew_opt="$brew/opt"
        if [ "$(arch)" = "arm64" ]; then
            # Workaround https://bugs.mysql.com/bug.php?id=113047 (MTR test
            # main.derived_limit fails with small cost differences) and
            # https://bugs.mysql.com/bug.php?id=113048 (MTR test
            # gis.gis_bugs_crashes fails with a result difference)
            mysql_add_comp_flags "8.1.0" "8.2.0" "cxx" "-ffp-contract=off"
            mysql_add_comp_flags "8.0.18" "8.0.35" "cxx" "-ffp-contract=off"
            mysql_add_cmake_flags "8.0.18" "8.0.28" "any" \
                                  "-DWITH_SSL=$brew_opt/openssl@1.1"
            # Workaround https://perconadev.atlassian.net/browse/PS-9034 (MyRocks
            # configuration failure on Linux virtualized on M1 Mac)
            declare -a fb_common=("-DROCKSDB_BUILD_ARCH=armv8.1-a+crypto")
            export MY8030_MAX_CORE_DUMP_FLAGS=(
                "-DWITH_DEVELOPER_ENTITLEMENTS=ON")
        else
            declare -a fb_common=()
            export MY8030_MAX_CORE_DUMP_FLAGS=()
        fi
        declare -a cmake_release=()
        mysql_add_comp_flags "9.0.0" "9.0.1" "cxx" "-Wno-unused-lambda-capture"
        mysql_add_cmake_flags "8.0.18" "8.0.18" "any" \
                              "-DWITH_ZSTD=bundled" "-DWITH_PROTOBUF=bundled"
        mysql_add_cmake_flags "8.0.18" "8.0.28" "any" \
                              "-DWITH_ICU=$brew_opt/icu4c"
        mysql_add_comp_flags \
            "8.0.18" "8.0.28" cxx \
            "-Wno-shadow-field" "-Wno-unqualified-std-cast-call" \
            "-Wno-deprecated-copy-with-dtor" "-Wno-bitwise-instead-of-logical" \
            "-Wno-unused-label" \
            `# LLVM 14:` \
            "-Wno-error=unused-variable" "-Wno-error=missing-noreturn" \
            "-Wno-error=extra-semi" \
            `# LLVM 14 RocksDB:` \
            "-Wno-error=invalid-offsetof" \
            "-Wno-error=conditional-uninitialized" \
            `# LLVM 17:` \
            "-Wno-error=implicit-const-int-float-conversion"
        mysql_add_comp_flags "8.0.31" "8.0.31" cxx "-Wno-deprecated-declarations"
        mysql_add_comp_flags "8.0.32" "8.0.34" cxx \
                             "-Wno-unused-but-set-variable" \
                             "-Wno-deprecated-copy-with-dtor"
        mysql_add_comp_flags "8.0.36" "8.0.36" cxx "-Wno-unused-variable"

        declare -a maria_common=(
            "-DCMAKE_C_FLAGS='-isystem /usr/local/include'"
            "-DCMAKE_CXX_FLAGS='-isystem /usr/local/include'")

        # Currently vectordb is only built on macOS. Only recognized starting
        # from 8.0.32, make it version-dependent if that becomes a problem, in
        # which case adjust the platform-specific fb_common values
        fb_common+=("-DWITH_FB_VECTORDB=ON"
                    "-DWITH_OPENMP=$brew_opt/libomp"
                    "-DWITH_OPENBLAS=$brew_opt/openblas")

        export MYCLANG12=("-DCMAKE_C_COMPILER=$brew_opt/llvm@12/bin/clang"
                          "-DCMAKE_CXX_COMPILER=$brew_opt/llvm@12/bin/clang++")
        export MYCLANG13=("-DCMAKE_C_COMPILER=$brew_opt/llvm@13/bin/clang"
                          "-DCMAKE_CXX_COMPILER=$brew_opt/llvm@13/bin/clang++")
        # CMAKE_AR settings below workaround
        # https://bugs.mysql.com/bug.php?id=113113 (Build failure with Homebrew
        # LLVM 14-17 on macOS). Remove the workaround when 8.4.4 and 9.2.0 are
        # the oldest supported versions.
        export MYCLANG14=("-DCMAKE_C_COMPILER=$brew_opt/llvm@14/bin/clang"
                          "-DCMAKE_CXX_COMPILER=$brew_opt/llvm@14/bin/clang++"
                          "-DCMAKE_AR=$brew_opt/llvm@14/bin/llvm-ar")
        export MYCLANG15=("-DCMAKE_C_COMPILER=$brew_opt/llvm@15/bin/clang"
                          "-DCMAKE_CXX_COMPILER=$brew_opt/llvm@15/bin/clang++"
                          "-DCMAKE_AR=$brew_opt/llvm@15/bin/llvm-ar")
        export MYCLANG16=("-DCMAKE_C_COMPILER=$brew_opt/llvm@16/bin/clang"
                          "-DCMAKE_CXX_COMPILER=$brew_opt/llvm@16/bin/clang++"
                          "-DCMAKE_AR=$brew_opt/llvm@16/bin/llvm-ar")
        export MYCLANG17=("-DCMAKE_C_COMPILER=$brew_opt/llvm@17/bin/clang"
                          "-DCMAKE_CXX_COMPILER=$brew_opt/llvm@17/bin/clang++"
                          "-DCMAKE_AR=$brew_opt/llvm@17/bin/llvm-ar")
        export MYCLANG18=("-DCMAKE_C_COMPILER=$brew_opt/llvm@18/bin/clang"
                          "-DCMAKE_CXX_COMPILER=$brew_opt/llvm@18/bin/clang++"
                          "-DCMAKE_AR=$brew_opt/llvm@18/bin/llvm-ar")
        export MYCLANG=("-DCMAKE_C_COMPILER=$brew_opt/llvm/bin/clang"
                        "-DCMAKE_CXX_COMPILER=$brew_opt/llvm/bin/clang++"
                        "-DCMAKE_AR=$brew_opt/llvm/bin/llvm-ar")
        export MYGCC11=("-DCMAKE_C_COMPILER=$brew/bin/gcc-11"
                        "-DCMAKE_CXX_COMPILER=$brew/bin/g++-11")
        export MYGCC12=("-DCMAKE_C_COMPILER=$brew/bin/gcc-12"
                        "-DCMAKE_CXX_COMPILER=$brew/bin/g++-12")
        export MYGCC13=("-DCMAKE_C_COMPILER=$brew/bin/gcc-13"
                        "-DCMAKE_CXX_COMPILER=$brew/bin/g++-13")
        export MYGCC14=("-DCMAKE_C_COMPILER=$brew/bin/gcc-14"
                        "-DCMAKE_CXX_COMPILER=$brew/bin/g++-14")

        declare -r emd_libdir="$brew_opt/libeatmydata/lib/"
        export MTR_EMD=(
            "--mysqld-env=DYLD_LIBRARY_PATH=$emd_libdir"
            "--mysqld-env=DYLD_FORCE_FLAT_NAMESPACE=1"
            "--mysqld-env=DYLD_INSERT_LIBRARIES=$emd_libdir/libeatmydata.dylib")
    else
        # Linux-only because of https://bugs.mysql.com/bug.php?id=115471
        declare -a cmake_release=("-DWITH_LTO=ON")
        # FIXME(laurynas): convert all warning -Wno- to -Wno-error=
        mysql_add_comp_flags "8.0.18" "8.0.28" cxx "-Wno-unused-label"
        # FIXME(laurynas): fix in fb-mysql?
        mysql_add_comp_flags "8.0.18" "8.0.35" cxx \
                             "-Wno-error=ignored-attributes"
        declare -a maria_common=()
        if [ "$(arch)" = "aarch64" ]; then
            # Workaround https://perconadev.atlassian.net/browse/PS-9034 (MyRocks
            # configuration failure on Linux virtualized on M1 Mac)
            declare -a fb_common=("-DROCKSDB_BUILD_ARCH=armv8.1-a+crypto")
        else
            declare -a fb_common=()
        fi

        export MY8030_MAX_CORE_DUMP_FLAGS=()
        export MYCLANG12=("-DCMAKE_C_COMPILER=clang-12"
                          "-DCMAKE_CXX_COMPILER=clang++-12")
        export MYCLANG13=("-DCMAKE_C_COMPILER=clang-13"
                          "-DCMAKE_CXX_COMPILER=clang++-13")
        export MYCLANG14=("-DCMAKE_C_COMPILER=clang-14"
                          "-DCMAKE_CXX_COMPILER=clang++-14")
        export MYCLANG15=("-DCMAKE_C_COMPILER=clang-15"
                          "-DCMAKE_CXX_COMPILER=clang++-15")
        export MYCLANG16=("-DCMAKE_C_COMPILER=clang-16"
                          "-DCMAKE_CXX_COMPILER=clang++-16")
        export MYCLANG17=("-DCMAKE_C_COMPILER=clang-17"
                          "-DCMAKE_CXX_COMPILER=clang++-17")
        export MYCLANG18=("-DCMAKE_C_COMPILER=clang-18"
                          "-DCMAKE_CXX_COMPILER=clang++-18")
        export MYCLANG=("-DCMAKE_C_COMPILER=clang"
                        "-DCMAKE_CXX_COMPILER=clang++")
        export MYGCC11=("-DCMAKE_C_COMPILER=gcc-11"
                        "-DCMAKE_CXX_COMPILER=g++-11")
        export MYGCC12=("-DCMAKE_C_COMPILER=gcc-12"
                        "-DCMAKE_CXX_COMPILER=g++-12")
        export MYGCC13=("-DCMAKE_C_COMPILER=gcc-13"
                        "-DCMAKE_CXX_COMPILER=g++-13")
        export MYGCC14=("-DCMAKE_C_COMPILER=gcc-14"
                        "-DCMAKE_CXX_COMPILER=g++-14")

        # FIXME(laurynas): incorrect for aarch64
        export MTR_EMD=(
            "--mysqld-env=LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libeatmydata.so")
    fi

    # Platform-independent helpers

    export MYSAN=("-DWITH_ASAN=ON" "-DWITH_ASAN_SCOPE=ON" "-DWITH_UBSAN=ON")

    # Common building blocks

    mysql_add_comp_flags "${min_version}" "${max_version}" "cxx" "-g"
    mysql_add_comp_flags "${min_version}" "${max_version}" "cxx_release" \
                         "-O2" "-DNDEBUG"

    declare -a -r cmake_common=("-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
                                "-DFORCE_UNSUPPORTED_COMPILER=ON")
    cmake_release+=("${cmake_common[@]}" "-DBUILD_CONFIG=mysql_release"
                    "-DCMAKE_BUILD_TYPE=Release")
    declare -a -r cmake_debug=("${cmake_common[@]}" "-DCMAKE_BUILD_TYPE=Debug"
                               "-DWITH_DEBUG=ON")

    declare -a -r my=("-DMYSQL_MAINTAINER_MODE=ON" "-DWITH_SYSTEM_LIBS=ON"
                      "-DWITH_NDBCLUSTER_STORAGE_ENGINE=OFF")
    declare -a -r myd=("${cmake_debug[@]}" "${my[@]}")
    declare -a -r myr=("${cmake_release[@]}" "${my[@]}")

    # Workaround Facebook tooling incompatibility with git worktrees
    fb_common+=("-DMYSQL_GITHASH=0" "-DMYSQL_GITDATE=2100-02-29"
                "-DROCKSDB_GITHASH=0" "-DROCKSDB_GITDATE=2100-02-29")

    maria_common+=("-DPLUGIN_MROONGA=NO" "-DPLUGIN_CONNECT=NO")

    # Version-specific building blocks, descending order

    mysql_add_cmake_flags "8.0.33" "${max_version}" any \
                          "-DFORCE_COLORED_OUTPUT=ON"

    # Workaround https://bugs.mysql.com/bug.php?id=117299 (ddl0bulk.h:236:15:
    # error: parameter 'prebuilt' not found in the function declaration
    # [-Werror,-Wdocumentation])
    mysql_add_comp_flags "9.2.0" "9.2.0" cxx "-Wno-documentation"

    declare -a -r my920_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 9.2.0 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 9.2.0 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 9.2.0 cxx_release)"
    )

    mysql_add_comp_flags "9.1.0" "9.1.0" cxx "-Wno-unused-const-variable"

    declare -a -r my910_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 9.1.0 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 9.1.0 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 9.1.0 cxx_release)"
    )

    declare -a -r my901_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 9.0.1 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 9.0.1 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 9.0.1 cxx_release)"
    )

    declare -a -r my900_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 9.0.0 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 9.0.0 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 9.0.0 cxx_release)"
    )

    declare -a -r my844_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.4.4 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.4.4 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.4.4 cxx_release)"
    )

    mysql_add_comp_flags "8.4.3" "8.4.3" cxx "-Wno-unused-const-variable"

    declare -a -r my843_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.4.3 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.4.3 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.4.3 cxx_release)"
    )

    declare -a -r my842_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.4.2 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.4.2 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.4.2 cxx_release)"
    )

    declare -a -r my841_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.4.1 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.4.1 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.4.1 cxx_release)"
    )

    declare -a -r my840_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.4.0 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.4.0 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.4.0 cxx_release)"
    )

    declare -a -r my830_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.3.0 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.3.0 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.3.0 cxx_release)"
    )

    mysql_add_cmake_flags "8.0.18" "8.2.0" any \
                          "-DDOWNLOAD_BOOST=ON" \
                          "-DWITH_BOOST=~/vilniusdb/mysql-boost/"

    declare -a -r my820_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.2.0 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.2.0 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.2.0 cxx_release)"
    )

    declare -a -r my810_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.1.0 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.1.0 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.1.0 cxx_release)"
    )

    declare -a -r my8041_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.0.41 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.0.41 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.0.41 cxx_release)"
    )

    declare -a -r my8040_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.0.40 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.0.40 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.0.40 cxx_release)"
    )

    declare -a -r my8039_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.0.39 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.0.39 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.0.39 cxx_release)"
    )

    declare -a -r my8038_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.0.38 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.0.38 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.0.38 cxx_release)"
    )

    declare -a -r my8037_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.0.37 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.0.37 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.0.37 cxx_release)"
    )

    declare -a -r my8036_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.0.36 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.0.36 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.0.36 cxx_release)"
    )

    mysql_add_cmake_flags "8.0.32" "8.0.36" any "-DWITH_UNIT_TESTS=OFF"

    declare -a -r my8035_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.0.35 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.0.35 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.0.35 cxx_release)"
    )

    declare -a -r my8034_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.0.34 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.0.34 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.0.34 cxx_release)"
    )

    declare -a -r my8033_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.0.33 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.0.33 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.0.33 cxx_release)"
    )

    mysql_add_cmake_flags "8.0.33" "8.0.33" any "-DWITH_RAPIDJSON=bundled"

    declare -a -r my8032_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.0.32 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.0.32 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.0.32 cxx_release)"
    )

    declare -a -r my8031_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.0.31 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.0.31 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.0.31 cxx_release)"
    )

    declare -a -r my8030_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.0.30 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.0.30 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.0.30 cxx_release)"
    )

    mysql_add_comp_flags "8.0.28" "8.0.29" cxx_debug \
                         "-Wno-deprecated-declarations"

    declare -a -r my8029_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.0.29 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.0.29 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.0.29 cxx_release)"
    )

    mysql_add_cmake_flags "8.0.27" "8.0.28" any "-DWITH_FIDO=bundled"

    mysql_add_comp_flags "8.0.18" "8.0.28" cxx_debug \
                         "-Wno-unknown-warning-option" \
                         "-Wno-unused-but-set-variable" \
                         "-Wno-discarded-qualifiers"

    mysql_add_cmake_flags "8.0.18" "8.0.28" "any" \
                          "-DWITH_RAPIDJSON=bundled" "-DWITH_LZ4=bundled"

    declare -a -r fb8028_extra=("-DWITH_UNIT_TESTS=OFF")

    declare -a -r my8028_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.0.28 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.0.28 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.0.28 cxx_release)"
    )

    # 8.0.27

    declare -a -r my8027_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.0.27 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.0.27 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.0.27 cxx_release)"
    )

    # 8.0.26

    declare -a -r my8026_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.0.26 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.0.26 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.0.26 cxx_release)"
    )

    mysql_add_cmake_flags "8.0.26" "8.0.26" any "-DENABLE_DOWNLOADS=ON"

    # 8.0.18

    mysql_add_comp_flags "8.0.18" "8.0.18" cxx \
                         "-Wno-deprecated-declarations" \
                         "-Wno-unused-result" \
                         "-Wno-range-loop-construct" \
                         "-Wno-non-c-typedef-for-linkage"

    declare -a -r my8018_comp_flags=(
        "-DCMAKE_CXX_FLAGS=$(mysql_get_comp_flags 8.0.18 cxx)"
        "-DCMAKE_CXX_FLAGS_DEBUG=$(mysql_get_comp_flags 8.0.18 cxx_debug)"
        "-DCMAKE_CXX_FLAGS_RELEASE=$(mysql_get_comp_flags 8.0.18 cxx_release)"
    )

    # Paydirt!

    export MY920D=("${myd[@]}" $(mysql_get_cmake_flags 9.2.0 any_debug)
                   "${my920_comp_flags[@]}")
    export MY920=("${myr[@]}" $(mysql_get_cmake_flags 9.2.0 any_release)
                  "${my920_comp_flags[@]}")

    export MY910D=("${myd[@]}" $(mysql_get_cmake_flags 9.1.0 any_debug)
                   "${my910_comp_flags[@]}")
    export MY910=("${myr[@]}" $(mysql_get_cmake_flags 9.1.0 any_release)
                  "${my910_comp_flags[@]}")

    export MY901D=("${myd[@]}" $(mysql_get_cmake_flags 9.0.1 any_debug)
                   "${my901_comp_flags[@]}")
    export MY901=("${myr[@]}" $(mysql_get_cmake_flags 9.0.1 any_release)
                  "${my901_comp_flags[@]}")

    export MY900D=("${myd[@]}" $(mysql_get_cmake_flags 9.0.0 any_debug)
                   "${my900_comp_flags[@]}")
    export MY900=("${myr[@]}" $(mysql_get_cmake_flags 9.0.0 any_release)
                  "${my900_comp_flags[@]}")

    export MY844D=("${myd[@]}" $(mysql_get_cmake_flags 8.4.4 any_debug)
                   "${my844_comp_flags[@]}")
    export MY844=("${myr[@]}" $(mysql_get_cmake_flags 8.4.4 any_release)
                  "${my844_comp_flags[@]}")

    export MY843D=("${myd[@]}" $(mysql_get_cmake_flags 8.4.3 any_debug)
                   "${my843_comp_flags[@]}")
    export MY843=("${myr[@]}" $(mysql_get_cmake_flags 8.4.3 any_release)
                  "${my843_comp_flags[@]}")

    export MY842D=("${myd[@]}" $(mysql_get_cmake_flags 8.4.2 any_debug)
                   "${my842_comp_flags[@]}")
    export MY842=("${myr[@]}" $(mysql_get_cmake_flags 8.4.2 any_release)
                  "${my842_comp_flags[@]}")

    export MY841D=("${myd[@]}" $(mysql_get_cmake_flags 8.4.1 any_debug)
                   "${my841_comp_flags[@]}")
    export MY841=("${myr[@]}" $(mysql_get_cmake_flags 8.4.1 any_release)
                  "${my841_comp_flags[@]}")

    export MY840D=("${myd[@]}" $(mysql_get_cmake_flags 8.4.0 any_debug)
                   "${my840_comp_flags[@]}")
    export MY840=("${myr[@]}" $(mysql_get_cmake_flags 8.4.0 any_release)
                  "${my841_comp_flags[@]}")

    export MY830D=("${myd[@]}" $(mysql_get_cmake_flags 8.3.0 any_debug)
                   "${my830_comp_flags[@]}")
    export MY830=("${myr[@]}" $(mysql_get_cmake_flags 8.3.0 any_release)
                  "${my830_comp_flags[@]}")

    export MY820D=("${myd[@]}" $(mysql_get_cmake_flags 8.2.0 any_debug)
                   "${my820_comp_flags[@]}")
    export MY820=("${myr[@]}" $(mysql_get_cmake_flags 8.2.0 any_release)
                  "${my820_comp_flags[@]}")

    export MY810D=("${myd[@]}" $(mysql_get_cmake_flags 8.1.0 any_debug)
                   "${my810_comp_flags[@]}")
    export MY810=("${myr[@]}" $(mysql_get_cmake_flags 8.1.0 any_release)
                   "${my810_comp_flags[@]}")

    export MY8041D=("${myd[@]}" $(mysql_get_cmake_flags 8.0.41 any_debug)
                    "${my8041_comp_flags[@]}")
    export MY8041=("${myr[@]}" $(mysql_get_cmake_flags 8.0.41 any_release)
                   "${my8041_comp_flags[@]}")

    export MY8040D=("${myd[@]}" $(mysql_get_cmake_flags 8.0.40 any_debug)
                    "${my8040_comp_flags[@]}")
    export MY8040=("${myr[@]}" $(mysql_get_cmake_flags 8.0.40 any_release)
                   "${my8040_comp_flags[@]}")

    export MY8039D=("${myd[@]}" $(mysql_get_cmake_flags 8.0.39 any_debug)
                    "${my8039_comp_flags[@]}")
    export MY8039=("${myr[@]}" $(mysql_get_cmake_flags 8.0.39 any_release)
                    "${my8039_comp_flags[@]}")

    export MY8038D=("${myd[@]}" $(mysql_get_cmake_flags 8.0.38 any_debug)
                    "${my8038_comp_flags[@]}")
    export MY8038=("${myr[@]}" $(mysql_get_cmake_flags 8.0.38 any_release)
                    "${my8038_comp_flags[@]}")

    export MY8037D=("${myd[@]}" $(mysql_get_cmake_flags 8.0.37 any_debug)
                    "${my8037_comp_flags[@]}")
    export MY8037=("${myr[@]}" $(mysql_get_cmake_flags 8.0.37 any_release)
                    "${my8037_comp_flags[@]}")

    export MY8036D=("${myd[@]}" $(mysql_get_cmake_flags 8.0.36 any_debug)
                    "${my8036_comp_flags[@]}")
    export MY8036=("${myr[@]}" $(mysql_get_cmake_flags 8.0.36 any_release)
                    "${my8036_comp_flags[@]}")

    export PS8036D=("${myd[@]}" $(mysql_get_cmake_flags 8.0.36 any_debug)
                    "${my8036_comp_flags[@]}")

    export FB8036D=("${MY8036D[@]}" "${fb_common[@]}")
    export FB8036=("${MY8036[@]}" "${fb_common[@]}")

    export MY8035D=("${myd[@]}" $(mysql_get_cmake_flags 8.0.35 any_debug)
                    "${my8035_comp_flags[@]}")
    export MY8035=("${myr[@]}" $(mysql_get_cmake_flags 8.0.35 any_release)
                   "${my8035_comp_flags[@]}")

    export PS8035D=("${myd[@]}" $(mysql_get_cmake_flags 8.0.35 any_debug)
                    "${my8035_comp_flags[@]}")

    export MY8034D=("${myd[@]}" $(mysql_get_cmake_flags 8.0.34 any_debug)
                    "${my8034_comp_flags[@]}")
    export MY8034=("${myr[@]}" $(mysql_get_cmake_flags 8.0.34 any_release)
                    "${my8034_comp_flags[@]}")

    export PS8034D=("${myd[@]}" $(mysql_get_cmake_flags 8.0.34 any_debug)
                    "${my8034_comp_flags[@]}")

    export MY8033D=("${myd[@]}" $(mysql_get_cmake_flags 8.0.33 any_debug)
                    "${my8033_comp_flags[@]}")
    export MY8033=("${myr[@]}" $(mysql_get_cmake_flags 8.0.33 any_release)
                   "${my8033_comp_flags[@]}")

    export MY8032D=("${myd[@]}" $(mysql_get_cmake_flags 8.0.32 any_debug)
                    "${my8032_comp_flags[@]}")
    export MY8032=("${myr[@]}" $(mysql_get_cmake_flags 8.0.32 any_release)
                   "${my8032_comp_flags[@]}")

    export FB8032D=("${MY8032D[@]}" "${fb_common[@]}")
    export FB8032=("${MY8032[@]}" "${fb_common[@]}")

    export MY8031D=("${myd[@]}" $(mysql_get_cmake_flags 8.0.31 any_debug)
                    "${my8031_comp_flags[@]}")

    export MY8030D=("${myd[@]}" $(mysql_get_cmake_flags 8.0.30 any_debug)
                    "${my8030_comp_flags[@]}")

    export MY8029D=("${myd[@]}" $(mysql_get_cmake_flags 8.0.29 any_debug)
                    "${my8029_comp_flags[@]}")

    export MY8028=("${myr[@]}" $(mysql_get_cmake_flags 8.0.28 any_release)
                   "${my8028_comp_flags[@]}")
    export MY8028D=("${myd[@]}" $(mysql_get_cmake_flags 8.0.28 any_debug)
                    "${my8028_comp_flags[@]}")

    export FB8028=("${MY8028[@]}" "${fb_common[@]}" "${fb8028_extra[@]}")
    export FB8028D=("${MY8028D[@]}" "${fb_common[@]}" "${fb8028_extra[@]}")

    export MY8027D=("${myd[@]}" $(mysql_get_cmake_flags 8.0.27 any_debug)
                    "${my8027_comp_flags[@]}")

    export MY8026=("${myr[@]}" $(mysql_get_cmake_flags 8.0.26 any_release)
                   "${my8027_comp_flags[@]}")
    export MY8026D=("${myd[@]}" $(mysql_get_cmake_flags 8.0.26 any_debug)
                    "-DDEBUG_EXTNAME=OFF" "${my8026_comp_flags[@]}")

    export FB8026=("${MY8026[@]}" "${fb_common[@]}")
    export FB8026D=("${MY8026D[@]}" "${fb_common[@]}")

    export MY8018=("${myr[@]}" $(mysql_get_cmake_flags 8.0.18 any_release)
                   "${my8018_comp_flags[@]}")
    export MY8018D=("${myd[@]}" $(mysql_get_cmake_flags 8.0.18 any_debug)
                    "${my8018_comp_flags[@]}")

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

# Any arguments will passed through to CMake
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

    if ! declare -i -r patch_level=$(mysql_get_patch_level "$version_file");
    then
        popd
        return 1
    fi

    declare -r flavor=$(mysql_determine_flavor "$version_file")

    popd || return 1

    case "$flavor" in
        "mysql")
            echo "Configuring MySQL $major_ver.$minor_ver.$patch_level"
            case "$major_ver.$minor_ver.$patch_level" in
                9.2.0)
                    declare -a release_flags=("${MY920[@]}")
                    declare -a debug_flags=("${MY920D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                9.1.0)
                    declare -a release_flags=("${MY910[@]}")
                    declare -a debug_flags=("${MY910D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                9.0.1)
                    declare -a release_flags=("${MY901[@]}")
                    declare -a debug_flags=("${MY901D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                9.0.0)
                    declare -a release_flags=("${MY900[@]}")
                    declare -a debug_flags=("${MY900D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.4.4)
                    declare -a release_flags=("${MY844[@]}")
                    declare -a debug_flags=("${MY844D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.4.3)
                    declare -a release_flags=("${MY843[@]}")
                    declare -a debug_flags=("${MY843D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.4.2)
                    declare -a release_flags=("${MY842[@]}")
                    declare -a debug_flags=("${MY842D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.4.1)
                    declare -a release_flags=("${MY841[@]}")
                    declare -a debug_flags=("${MY841D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.4.0)
                    declare -a release_flags=("${MY840[@]}")
                    declare -a debug_flags=("${MY840D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.3.0)
                    declare -a release_flags=("${MY830[@]}")
                    declare -a debug_flags=("${MY830D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.2.0)
                    declare -a release_flags=("${MY820[@]}")
                    declare -a debug_flags=("${MY820D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.1.0)
                    declare -a release_flags=("${MY810[@]}")
                    declare -a debug_flags=("${MY810D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.41)
                    declare -a release_flags=("${MY8041[@]}")
                    declare -a debug_flags=("${MY8041D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.40)
                    declare -a release_flags=("${MY8040[@]}")
                    declare -a debug_flags=("${MY8040D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.39)
                    declare -a release_flags=("${MY8039[@]}")
                    declare -a debug_flags=("${MY8039D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.38)
                    declare -a release_flags=("${MY8038[@]}")
                    declare -a debug_flags=("${MY8038D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.37)
                    declare -a release_flags=("${MY8037[@]}")
                    declare -a debug_flags=("${MY8037D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.36)
                    declare -a release_flags=("${MY8036[@]}")
                    declare -a debug_flags=("${MY8036D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.35)
                    declare -a release_flags=("${MY8035[@]}")
                    declare -a debug_flags=("${MY8035D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.34)
                    declare -a release_flags=("${MY8034[@]}")
                    declare -a debug_flags=("${MY8034D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.33)
                    declare -a release_flags=("${MY8033[@]}")
                    declare -a debug_flags=("${MY8033D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.32)
                    declare -a release_flags=("${MY8032[@]}")
                    declare -a debug_flags=("${MY8032D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.31)
                    declare -a release_flags=("${MY8031[@]}")
                    declare -a debug_flags=("${MY8031D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.30)
                    declare -a release_flags=("${MY8030[@]}")
                    declare -a debug_flags=("${MY8030D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
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
                8.0.36)
                    declare -a release_flags=("${FB8036[@]}")
                    declare -a debug_flags=("${FB8036D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.32)
                    declare -a release_flags=("${FB8032[@]}")
                    declare -a debug_flags=("${FB8032D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
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
                8.0.36)
                    declare -a debug_flags=("${PS8036D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.35)
                    declare -a debug_flags=("${PS8035D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
                    ;;
                8.0.34)
                    declare -a debug_flags=("${PS8034D[@]}")
                    declare -a -r \
                            core_dump_flags=("${MY8030_MAX_CORE_DUMP_FLAGS[@]}")
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

    # build directory can be named arbitrarily as long as it starts with a
    # $MY_BUILD_DIR_PREFIX. The build type is determined by the presence of
    # certain substrings in the name.
    declare -r build_dir="$(basename "$PWD")"

    case "$build_dir" in
        *san*)
            echo "Using sanitizers"
            declare -i -r sanitizers=1
            debug_flags+=("${MYSAN[@]}")
            release_flags+=("${MYSAN[@]}")
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
        *llvm-18*)
            echo "Using LLVM 18"
            debug_flags+=("${MYCLANG18[@]}")
            release_flags+=("${MYCLANG18[@]}")
            ;;
        *llvm*)
            echo "Using LLVM"
            debug_flags+=("${MYCLANG[@]}")
            release_flags+=("${MYCLANG[@]}")
            ;;
        *gcc-11*)
            echo "Using GCC 11"
            debug_flags+=("${MYGCC11[@]}")
            release_flags+=("${MYGCC11[@]}")
            ;;
        *gcc-12*)
            echo "Using GCC 12"
            debug_flags+=("${MYGCC12[@]}")
            release_flags+=("${MYGCC12[@]}")
            ;;
        *gcc-13*)
            echo "Using GCC 13"
            debug_flags+=("${MYGCC13[@]}")
            release_flags+=("${MYGCC13[@]}")
            ;;
        *gcc-14*)
            echo "Using GCC 14"
            debug_flags+=("${MYGCC14[@]}")
            release_flags+=("${MYGCC14[@]}")
            ;;
    esac

    # TODO(laurynas): we might want to brew unlink boost temporarily for
    # configuration and build, but each of these functions may be called
    # independently. Re-linking at the end of configuration just to unlink again
    # at the start of the build would work, but it is not nice.

    case "$build_dir" in
        *debug*)
            echo "Debug build"
            echo "CMake options: ${debug_flags[@]} $@"
            cmake -G Ninja .. "${debug_flags[@]}" "$@"
            ;;
        *release*)
            echo "Release build"
            echo "CMake options: ${release_flags[@]} $@"
            cmake -G Ninja .. "${release_flags[@]}" "$@"
            ;;
        *)
            2>&1 echo "Must choose either debug or release build"
            return 1
            ;;
    esac
}

# Incremental rebuild command, should be called from the build directory one
# level below the source tree.
# TODO(laurynas): make it work from two levels below the source tree too (for
# example _build-debug/mysql-test)
mysql_rebuild() {
    pushd .. || return 1
    if ! declare -i -r workaround_ssl3=$(mysql_need_openssl3_workaround); then
        popd
        return 1
    fi
    popd || return 1

    rm -f dev.entitlements
    mysql_maybe_workaround_openssl3 $workaround_ssl3
    ninja
    declare -i -r ninja_status=$?
    mysql_undo_openssl3_workaround $workaround_ssl3

    return ninja_status
}

mysql_build_in_build_dir() {
    git submodule update --init --recursive || return $?
    mysql_cmake "$@" || return $?

    declare -r build_dir="$(basename "$PWD")"

    pushd .. || return 1

    # TODO(laurynas): an option for temp builds that does not touch the symlink
    ln -sf "$build_dir/compile_commands.json" .

    popd || return 1

    mysql_rebuild
}

mysql_cd_into_build_dir() {
    declare -r build_type=$1
    declare -r build_dir="$MY_BUILD_DIR_PREFIX$build_type"
    mkdir -p "$build_dir"
    cd "$build_dir" || return 1
}

mysql_build() {
    declare -r build_type_arg=$1
    declare -r cur_dir_name="$(basename "$PWD")"

    if [ -n "$build_type_arg" ]; then
        if [[ $cur_dir_name == "$MY_BUILD_DIR_PREFIX"* ]]; then
            2>&1 echo "Build type given but already in a build dir"
            return 1
        fi
        mysql_cd_into_build_dir "$build_type_arg" || return 1
    elif [[ $cur_dir_name != "$MY_BUILD_DIR_PREFIX"* ]]; then
        mysql_cd_into_build_dir "$MY_DEFAULT_BUILD_TYPE" || return 1
    fi

    mysql_build_in_build_dir
}

# Must be called from the directory containing all the build directories
for_all_build_dirs() {
    local command="$1"
    for build_dir in "$MY_BUILD_DIR_PREFIX"*; do
        if [ ! -d "$build_dir" ]; then
            continue
        fi

        pushd "$build_dir" || return 1

        $command

        declare -i build_status=$?
        popd || return 1
        if [ $build_status -ne 0 ]; then
            return $build_status
        fi
    done
}

# Must be called from the directory containing all the build directories
mysql_rebuild_all() {
    for_all_build_dirs mysql_rebuild
}

# Must be called from the directory containing all the build directories
mysql_build_all() {
    for_all_build_dirs mysql_build
}

mtr() {
    declare -r mktemp_template="/tmp/mtr-XXXX"
    # The temp directory is left around for both successful and error returns in
    # case a run needs to be reviewed or debugged.
    declare -r mtr_emd_tmp_dir=$(mktemp -d "$mktemp_template") ||
        { 2>&1 echo "Failed to create a temp dir like $mktemp_template";
          return 1; }

    # Disable malloc none zone to get rid of many "malloc: nano zone abandoned
    # due to inability to preallocate reserved vm space" messages
    # Disable ASan ODR violation detection until
    # https://bugs.mysql.com/bug.php?id=116372 is fixed
    MallocNanoZone=0 ASAN_OPTIONS="detect_odr_violation=0" \
        ./mtr "${MTR_EMD[@]}" --tmpdir="$mtr_emd_tmp_dir" "$@" || return $?
}

rmtr() {
    pushd .. || return 1
    mysql_rebuild
    declare -i -r build_status=$?
    popd || return 1
    if [ $build_status -ne 0 ]; then
        return $build_status
    fi

    mtr "$@"
}

rm_tmp_mtr() {
    killall -9 mysql_server_mock 2>/dev/null
    killall -9 mysqlrouter 2>/dev/null
    rm -rf /tmp/mtr-* /tmp/router-* /tmp/mysqld-* /tmp/mysql-unique-ids \
       /tmp/mysqlx.sock /tmp/mysqlx.sock.lock
}

rm_cores_tmp_mtr() {
    rmcores
    rm_tmp_mtr
}

cmtr() {
    rm_cores_tmp_mtr
    mtr "$@"
}

crmtr() {
    rm_cores_tmp_mtr
    rmtr "$@"
}

rcmtr() {
    crmtr "$@"
}

# macOS specific
lldb_mysqld_core() {
    declare -a -r cores=(/cores/*(N))
    [[ ${#cores[@]} -ne 1 ]] && {
        2>&1 echo "Error: not exactly one core file in /cores";
        return 1;
    }
    lldb ../runtime_output_directory/mysqld -c "${cores[1]}"
}

mysql_export_build_defaults
mysql_export_environment_helpers

# Workaround P10K going crazy
set +o errexit
set +o nounset
