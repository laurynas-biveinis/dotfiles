#!/bin/zsh
#
# Exported environment variables, CMake options:
# MY80xy, MY80xyD: Oracle MySQL release & debug build, for xy patchlevel version
# MARIA108, MARIA108D: MariaDB 10.8 release and debug build
# FB80xy, FB80xyD: MySQL 8.0 Facebook Patch release and debug
#
# Extra CMake options:
# MYCLANG, MYCLANG12, MYCLANG12: set compiler to clang for CMake
# MY80SAN: add maximum supported Sanitizer configuration
#
# mysql-test-run options:
# MTR_EMD: MTR options to preload libeatmydata
#
# Shell functions:
# - mtr_emd: call mtr with libeatmydata and some tmpdir
# - rm_tmp_mtr: cleanup all tmpdir from the above
# - mysql_cmake: in a build directory, figure out CMake options & run it
# - mysql_build: in a build directory, CMake and make
#
# Works on Linux and macOS.

set -o errexit
set -o nounset
set -o pipefail

UNAME_OUT="$(uname -s)"

# Common building blocks

if [ "$UNAME_OUT" = "Darwin" ]; then
    if [ "$(arch)" = "arm64" ]; then
        BREW="/opt/homebrew/opt"
        MY8026_28_EXTRA=("-DWITH_SSL=$BREW/openssl@1.1")
        MY8030_33_EXTRA=("-DWITH_DEVELOPER_ENTITLEMENTS=ON")
    else
        MY8026_28_EXTRA=()
        MY8030_33_EXTRA=()
    fi
    MY8026_28_EXTRA+=("-DWITH_ICU=$BREW/icu4c")
    MY8026_28_EXTRA_CXX_FLAGS=("-Wno-shadow-field")
    MY8031_EXTRA_CXX_FLAGS=("-Wno-deprecated-declarations")
    MY8032_EXTRA_CXX_FLAGS=("-Wno-unused-but-set-variable"
                            "-Wno-deprecated-copy-with-dtor")
    MY8033_EXTRA_CXX_FLAGS=("-Wno-unused-but-set-variable"
                            "-Wno-deprecated-copy-with-dtor")

    MARIA_COMMON=("-DCMAKE_C_FLAGS='-isystem /usr/local/include'"
                  "-DCMAKE_CXX_FLAGS='-isystem /usr/local/include'")

    FB_COMMON=()
    FB8028_EXTRA=("-DWITH_UNIT_TESTS=OFF")

    export MYCLANG12=("-DCMAKE_C_COMPILER=$BREW/llvm@12/bin/clang-12"
                      "-DCMAKE_CXX_COMPILER=$BREW/llvm@12/bin/clang++")
else
    MY8026_28_EXTRA=()
    MY8026_28_EXTRA_CXX_FLAGS=("-Wno-unused-label")
    MY8031_EXTRA_CXX_FLAGS=()
    MY8032_EXTRA_CXX_FLAGS=()
    MY8033_EXTRA_CXX_FLAGS=()
    MY8030_33_EXTRA=()
    MARIA_COMMON=()

    FB_COMMON=("-DWITH_ZSTD=bundled" "-DWITH_PROTOBUF=bundled")
    FB8028_EXTRA=("-DWITH_UNIT_TESTS=OFF")

    export MYCLANG12=("-DCMAKE_C_COMPILER=clang-12"
                      "-DCMAKE_CXX_COMPILER=clang++-12")
fi

CXX_FLAGS_DEBUG=("-g")
CXX_FLAGS_RELEASE=("-O2" "-g" "-DNDEBUG")

CMAKE_COMMON=("-DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
CMAKE_RELEASE=("${CMAKE_COMMON[@]}"
               "-DBUILD_CONFIG=mysql_release" "-DCMAKE_BUILD_TYPE=Release")
CMAKE_DEBUG=("${CMAKE_COMMON[@]}" "-DCMAKE_BUILD_TYPE=Debug" "-DWITH_DEBUG=ON")
unset CMAKE_COMMON

MY80=("-DMYSQL_MAINTAINER_MODE=ON" "-DDOWNLOAD_BOOST=ON"
      "-DWITH_BOOST=~/vilniusdb/mysql-boost/" "-DWITH_SYSTEM_LIBS=ON")
MY80D=("${CMAKE_DEBUG[@]}" "${MY80[@]}")
MY80R=("${CMAKE_RELEASE[@]}" "${MY80[@]}")
unset MY80

# Workaround Facebook tooling incompatibility with git worktrees
FB_COMMON+=("-DMYSQL_GITHASH=0" "-DMYSQL_GITDATE=2100-02-29"
            "-DROCKSDB_GITHASH=0" "-DROCKSDB_GITDATE=2100-02-29")

MARIA_COMMON+=("-DPLUGIN_MROONGA=NO" "-DPLUGIN_CONNECT=NO")

# Version-specific building blocks, descending order

MY8033_EXTRA=()

MY8033_CXX_FLAGS_DEBUG=("${MY8033_EXTRA_CXX_FLAGS[@]}" "${CXX_FLAGS_DEBUG[@]}")
MY8033_CXX_FLAGS_RELEASE=("${MY8033_EXTRA_CXX_FLAGS[@]}"
                          "${CXX_FLAGS_RELEASE[@]}")
MY8033_EXTRA=("-DWITH_RAPIDJSON=bundled" "-DFORCE_COLORED_OUTPUT=ON"
              "-DCMAKE_CXX_FLAGS=${MY8033_EXTRA_CXX_FLAGS[*]}"
              "-DCMAKE_C_FLAGS_DEBUG=${MY8033_CXX_FLAGS_DEBUG[*]}"
              "-DCMAKE_CXX_FLAGS_DEBUG=${MY8033_CXX_FLAGS_DEBUG[*]}"
              "-DCMAKE_CXX_FLAGS_RELEASE=${MY8033_CXX_FLAGS_RELEASE[*]}")
unset MY8033_EXTRA_CXX_FLAGS
unset MY8033_CXX_FLAGS_DEBUG
unset MY8033_CXX_FLAGS_RELEASE

MY8032_CXX_FLAGS_DEBUG=("${MY8032_EXTRA_CXX_FLAGS[@]}" "${CXX_FLAGS_DEBUG[@]}")
MY8032_CXX_FLAGS_RELEASE=("${MY8032_EXTRA_CXX_FLAGS[@]}"
                          "${CXX_FLAGS_RELEASE[@]}")
MY8032_EXTRA=("-DCMAKE_CXX_FLAGS=${MY8032_EXTRA_CXX_FLAGS[*]}"
              "-DCMAKE_C_FLAGS_DEBUG=${MY8032_CXX_FLAGS_DEBUG[*]}"
              "-DCMAKE_CXX_FLAGS_DEBUG=${MY8032_CXX_FLAGS_DEBUG[*]}"
              "-DCMAKE_CXX_FLAGS_RELEASE=${MY8032_CXX_FLAGS_RELEASE[*]}")
unset MY8032_EXTRA_CXX_FLAGS
unset MY8032_CXX_FLAGS_DEBUG
unset MY8032_CXX_FLAGS_RELEASE

MY8031_CXX_FLAGS_DEBUG=("${MY8031_EXTRA_CXX_FLAGS[@]}" "${CXX_FLAGS_DEBUG[@]}")
MY8031_CXX_FLAGS_RELEASE=("${MY8031_EXTRA_CXX_FLAGS[@]}"
                          "${CXX_FLAGS_RELEASE[@]}")
MY8031_EXTRA=("-DCMAKE_CXX_FLAGS=${MY8031_EXTRA_CXX_FLAGS[*]}"
              "-DCMAKE_C_FLAGS_DEBUG=${MY8031_CXX_FLAGS_DEBUG[*]}"
              "-DCMAKE_CXX_FLAGS_DEBUG=${MY8031_CXX_FLAGS_DEBUG[*]}"
              "-DCMAKE_CXX_FLAGS_RELEASE=${MY8031_CXX_FLAGS_RELEASE[*]}")
unset MY8031_EXTRA_CXX_FLAGS
unset MY8031_CXX_FLAGS_DEBUG
unset MY8031_CXX_FLAGS_RELEASE

MY8028_29_CXX_FLAGS_DEBUG=("${CXX_FLAGS_DEBUG[@]}"
                           "-Wno-deprecated-declarations")
MY8028_29_EXTRA=("-DCMAKE_C_FLAGS_DEBUG=${MY8028_29_CXX_FLAGS_DEBUG[*]}"
                 "-DCMAKE_CXX_FLAGS_DEBUG=${MY8028_29_CXX_FLAGS_DEBUG[*]}")
unset MY8028_29_CXX_FLAGS_DEBUG

MY8027_28_EXTRA="-DWITH_FIDO=bundled"

MY8026_28_C_FLAGS_DEBUG=("${MY8026_28_EXTRA_CXX_FLAGS[@]}"
                         "${CXX_FLAGS_DEBUG[@]}"
                         "-Wno-unknown-warning-option"
                         "-Wno-unused-but-set-variable"
                         "-Wno-discarded-qualifiers")
MY8026_28_CXX_FLAGS_DEBUG=("${MY8026_28_EXTRA_CXX_FLAGS[@]}"
                           "${CXX_FLAGS_DEBUG[@]}"
                           "-Wno-unknown-warning-option"
                           "-Wno-unused-but-set-variable")
MY8026_28_CXX_FLAGS_RELEASE=("${MY8026_28_EXTRA_CXX_FLAGS[@]}"
                             "${CXX_FLAGS_RELEASE[@]}")
MY8026_28_EXTRA+=("-DWITH_RAPIDJSON=bundled" "-DWITH_LZ4=bundled"
                  "-DCMAKE_CXX_FLAGS=${MY8026_28_EXTRA_CXX_FLAGS[*]}"
                  "-DCMAKE_C_FLAGS_DEBUG=${MY8026_28_C_FLAGS_DEBUG[*]}"
                  "-DCMAKE_CXX_FLAGS_DEBUG=${MY8026_28_CXX_FLAGS_DEBUG[*]}"
                  "-DCMAKE_CXX_FLAGS_RELEASE=${MY8026_28_CXX_FLAGS_RELEASE[*]}")
unset MY8026_28_C_FLAGS_DEBUG
unset MY8026_28_CXX_FLAGS_DEBUG
unset MY8026_28_CXX_FLAGS_RELEASE
unset MY8026_28_EXTRA_CXX_FLAGS
unset CXX_FLAGS_DEBUG
unset CXX_FLAGS_RELEASE

MY8026_EXTRA=("-DENABLE_DOWNLOADS=ON")

# Paydirt!

export MY8033D=("${MY80D[@]}" "${MY8033_EXTRA[@]}" "${MY8030_33_EXTRA[@]}")
export MY8033=("${MY80R[@]}" "${MY8033_EXTRA[@]}" "${MY8030_33_EXTRA[@]}")
unset MY8033_EXTRA

export MY8032D=("${MY80D[@]}" "${MY8032_EXTRA[@]}" "${MY8030_33_EXTRA[@]}")
export MY8032=("${MY80R[@]}" "${MY8032_EXTRA[@]}" "${MY8030_33_EXTRA[@]}")
unset MY8032_EXTRA

export MY8031D=("${MY80D[@]}" "${MY8031_EXTRA[@]}" "${MY8030_33_EXTRA[@]}")
unset MY8031_EXTRA

export MY8030D=("${MY80D[@]}" "${MY8030_33_EXTRA[@]}")
unset MY8030_33_EXTRA

export MY8029D=("${MY80D[@]}" "${MY8028_29_EXTRA[@]}")

export MY8028=("${MY80R[@]}" "${MY8026_28_EXTRA[@]}" "${MY8027_28_EXTRA[@]}"
               "${MY8028_29_EXTRA[@]}")
export MY8028D=("${MY80D[@]}" "${MY8026_28_EXTRA[@]}" "${MY8027_28_EXTRA[@]}"
                "${MY8028_29_EXTRA[@]}")
unset MY8028_29_EXTRA
unset MY8028_EXTRA

export FB8028=("${MY8028[@]}" "${FB_COMMON[@]}" "${FB8028_EXTRA[@]}")
export FB8028D=("${MY8028D[@]}" "${FB_COMMON[@]}" "${FB8028_EXTRA[@]}")
unset FB8028_EXTRA

export MY8027D=("${MY80D[@]}" "${MY8026_28_EXTRA[@]}" "${MY8027_28_EXTRA[@]}")
unset MY8027_28_EXTRA

export MY8026=("${MY80R[@]}" "${MY8026_28_EXTRA[@]}")
export MY8026D=("${MY80D[@]}" "-DDEBUG_EXTNAME=OFF" "${MY8026_28_EXTRA[@]}")
unset MY8026_28_EXTRA
unset MY80D
unset MY80R

export FB8026=("${MY8026[@]}" "${FB_COMMON[@]}")
export FB8026D=("${MY8026D[@]}" "${FB_COMMON[@]}")

export MARIA108=("${CMAKE_RELEASE[@]}" "${MARIA_COMMON[@]}")
export MARIA108D=("${CMAKE_DEBUG[@]}" "${MARIA_COMMON[@]}")

unset CMAKE_RELEASE
unset CMAKE_DEBUG

# Addons, environment helpers
export MYCLANG=("-DCMAKE_C_COMPILER=clang" "-DCMAKE_CXX_COMPILER=clang++")
export MYCLANG13=("-DCMAKE_C_COMPILER=clang-13"
                  "-DCMAKE_CXX_COMPILER=clang++-13")
export MY80SAN=("-DWITH_ASAN=ON" "-DWITH_ASAN_SCOPE=ON" "-DWITH_UBSAN=ON")

if [ "$UNAME_OUT" = "Darwin" ]; then
    EMD_LIBDIR="$BREW/libeatmydata/lib/"
    unset BREW
    export MTR_EMD=(
        "--mysqld-env=DYLD_LIBRARY_PATH=$EMD_LIBDIR"
        "--mysqld-env=DYLD_FORCE_FLAT_NAMESPACE=1"
        "--mysqld-env=DYLD_INSERT_LIBRARIES=$EMD_LIBDIR/libeatmydata.dylib")
else
    export MTR_EMD=(
        "--mysqld-env=LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libeatmydata.so")
fi

unset UNAME_OUT

mtr_emd() {
    mtr_emd_tmp_dir=$(mktemp -d /tmp/mtr-XXXX)
    ./mtr "${MTR_EMD[@]}" --tmpdir="$mtr_emd_tmp_dir" "$@"
}

rm_tmp_mtr() {
    rm -rf /tmp/mtr-*
}

mysql_cmake() {
    if [ -f ../MYSQL_VERSION ]; then
        major_ver_str=$(grep MYSQL_VERSION_MAJOR ../MYSQL_VERSION)
        major_ver="${major_ver_str//[^0-9]/}"
        if [ "$major_ver" != 8 ]; then
            echo "Only MySQL version 8 is supported"
            return
        fi
        patch_level_str=$(grep MYSQL_VERSION_PATCH ../MYSQL_VERSION)
        patch_level="${patch_level_str//[^0-9]/}"

        if [ -d ../rocksdb ]; then
            echo "Configuring Facebook MySQL $major_ver.0.$patch_level"
            case $patch_level in
                28)
                    release_flags=("${FB8028[@]}")
                    debug_flags=("${FB8028D[@]}")
                    ;;
                *)
                    echo "Unsupported version, please add"
                    return
                    ;;
            esac
        else
            echo "Configuring MySQL $major_ver.0.$patch_level"
            case $patch_level in
                33)
                    release_flags=("${MY8033[@]}")
                    debug_flags=("${MY8033D[@]}")
                    ;;
                32)
                    release_flags=("${MY8032[@]}")
                    debug_flags=("${MY8032D[@]}")
                    ;;
                28)
                    release_flags=("${MY8028[@]}")
                    debug_flags=("${MY8028D[@]}")
                    ;;
                *)
                    echo "Unsupported version, please add"
                    return
                    ;;
            esac
        fi
    elif [ -f ../VERSION ]; then
        echo "Configuring MariaDB, not implemented yet"
    else
        echo "Neither MariaDB nor MySQL source tree"
        return
    fi

    build_dir="$(basename "$PWD")"

    case "$build_dir" in
        *san*)
            echo "Using sanitizers"
            sanitizers=1
            debug_flags+=("${MY80SAN[@]}")
            release_flags+=("${MY80SAN[@]}")
            ;;
        *)
            sanitizers=0
            ;;
    esac

    case "$build_dir" in
        *valgrind*)
            echo "Using Valgrind"
            if [ "$sanitizers" == 1 ]; then
                echo "Valgrind is incompatible with sanitizers"
                return
            fi
            debug_flags+=("-DWITH_VALGRIND=ON")
            release_flags+=("-DWITH_VALGRIND=ON")
            ;;
    esac

    case "$build_dir" in
        *llvm*)
            echo "Using LLVM"
            debug_flags+=("${MYCLANG[@]}")
            release_flags+=("${MYCLANG[@]}")
            ;;
        *llvm-12*)
            echo "Using LLVM 12"
            debug_flags+=("${MYCLANG12[@]}")
            release_flags+=("${MYCLANG12[@]}")
            ;;
    esac

    case "$build_dir" in
        *debug*)
            echo "Debug build"
            cmake .. "${debug_flags[@]}" "$@"
            ;;
        *release*)
            echo "Release build"
            cmake .. "${release_flags[@]}" "$@"
            ;;
    esac
    set +x
}

mysql_build() {
    mysql_cmake "$@"
    (cd .. && ln -sf "$build_dir/compile_commands.json" .)
    make -j "$MAKE_J"
    build_dir="$(basename "$PWD")"
}

# Workaround P10K going crazy
set +o errexit
set +o nounset
