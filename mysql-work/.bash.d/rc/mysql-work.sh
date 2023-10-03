#!/bin/zsh
#
# Exported environment variables, CMake options:
# MY80xy, MY80xyD: Oracle MySQL release & debug build, for xy patchlevel version
# MARIA108, MARIA108D: MariaDB 10.8 release and debug build
# FB80xy, FB80xyD: MySQL 8.0 Facebook Patch release and debug
#
# Extra CMake options:
# MYCLANG, MYCLANG12, MYCLANG12: set compiler to clang for CMake
# MY8SAN: add maximum supported Sanitizer configuration
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
# Works on Linux and macOS (both Intel and Apple Silicon).

set -o errexit
set -o nounset
set -o pipefail

UNAME_OUT="$(uname -s)"

# Common building blocks

if [ "$UNAME_OUT" = "Darwin" ]; then
    if [ "$(arch)" = "arm64" ]; then
        BREW="/opt/homebrew/opt"
        MY8018_28_EXTRA=("-DWITH_SSL=$BREW/openssl@1.1")
        MY8030_810_EXTRA=("-DWITH_DEVELOPER_ENTITLEMENTS=ON")
        # Workaround https://jira.percona.com/browse/PS-8949
        FB8032_EXTRA=("-DROCKSDB_BUILD_ARCH=native+crypto")
        PS8034_EXTRA=("-DROCKSDB_BUILD_ARCH=native+crypto")
    else
        MY8018_28_EXTRA=()
        MY8030_810_EXTRA=()
        FB8032_EXTRA=()
        PS8034_EXTRA=()
    fi
    MY8018_EXTRA=("-DWITH_ZSTD=bundled" "-DWITH_PROTOBUF=bundled")
    MY8018_28_EXTRA+=("-DWITH_ICU=$BREW/icu4c")
    MY8018_28_EXTRA_CXX_FLAGS=("-Wno-shadow-field"
                               "-Wno-unqualified-std-cast-call"
                               "-Wno-deprecated-copy-with-dtor"
                               "-Wno-bitwise-instead-of-logical"
                               "-Wno-unused-label")
    MY8031_EXTRA_CXX_FLAGS=("-Wno-deprecated-declarations")
    MY8032_34_EXTRA_CXX_FLAGS=("-Wno-unused-but-set-variable"
                               "-Wno-deprecated-copy-with-dtor")
    MY8032_EXTRA=("-DWITH_UNIT_TESTS=OFF")

    MARIA_COMMON=("-DCMAKE_C_FLAGS='-isystem /usr/local/include'"
                  "-DCMAKE_CXX_FLAGS='-isystem /usr/local/include'")

    export MYCLANG12=("-DCMAKE_C_COMPILER=$BREW/llvm@12/bin/clang-12"
                      "-DCMAKE_CXX_COMPILER=$BREW/llvm@12/bin/clang++")
else
    MY8018_EXTRA=()
    MY8018_28_EXTRA=()
    MY8018_28_EXTRA_CXX_FLAGS=("-Wno-unused-label")
    MY8031_EXTRA_CXX_FLAGS=()
    MY8032_34_EXTRA_CXX_FLAGS=()
    MY8032_EXTRA=()
    FB8032_EXTRA=()
    PS8034_EXTRA=()
    MY8030_810_EXTRA=()
    MARIA_COMMON=()

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

MY8=("-DMYSQL_MAINTAINER_MODE=ON" "-DDOWNLOAD_BOOST=ON"
     "-DWITH_BOOST=~/vilniusdb/mysql-boost/" "-DWITH_SYSTEM_LIBS=ON")
MY8D=("${CMAKE_DEBUG[@]}" "${MY8[@]}")
MY8R=("${CMAKE_RELEASE[@]}" "${MY8[@]}")
unset MY8

# Workaround Facebook tooling incompatibility with git worktrees
FB_COMMON=("-DMYSQL_GITHASH=0" "-DMYSQL_GITDATE=2100-02-29"
           "-DROCKSDB_GITHASH=0" "-DROCKSDB_GITDATE=2100-02-29")

MARIA_COMMON+=("-DPLUGIN_MROONGA=NO" "-DPLUGIN_CONNECT=NO")

# Version-specific building blocks, descending order

# 8.0.34--8.0.33

MY8033_34_EXTRA=("-DFORCE_COLORED_OUTPUT=ON")

# 8.0.34

MY8034_CXX_FLAGS_DEBUG=("${MY8032_34_EXTRA_CXX_FLAGS[@]}" "${CXX_FLAGS_DEBUG[@]}")
MY8034_CXX_FLAGS_RELEASE=("${MY8032_34_EXTRA_CXX_FLAGS[@]}"
                          "${CXX_FLAGS_RELEASE[@]}")
MY8034_EXTRA=("-DCMAKE_CXX_FLAGS=${MY8032_34_EXTRA_CXX_FLAGS[*]}"
              "-DCMAKE_C_FLAGS_DEBUG=${MY8034_CXX_FLAGS_DEBUG[*]}"
              "-DCMAKE_CXX_FLAGS_DEBUG=${MY8034_CXX_FLAGS_DEBUG[*]}"
              "-DCMAKE_CXX_FLAGS_RELEASE=${MY8034_CXX_FLAGS_RELEASE[*]}")
unset MY8034_CXX_FLAGS_DEBUG
unset MY8034_CXX_FLAGS_RELEASE

# 8.0.33

MY8033_CXX_FLAGS_DEBUG=("${MY8032_34_EXTRA_CXX_FLAGS[@]}" "${CXX_FLAGS_DEBUG[@]}")
MY8033_CXX_FLAGS_RELEASE=("${MY8032_34_EXTRA_CXX_FLAGS[@]}"
                          "${CXX_FLAGS_RELEASE[@]}")
MY8033_EXTRA=("-DWITH_RAPIDJSON=bundled"
              "-DCMAKE_CXX_FLAGS=${MY8032_34_EXTRA_CXX_FLAGS[*]}"
              "-DCMAKE_C_FLAGS_DEBUG=${MY8033_CXX_FLAGS_DEBUG[*]}"
              "-DCMAKE_CXX_FLAGS_DEBUG=${MY8033_CXX_FLAGS_DEBUG[*]}"
              "-DCMAKE_CXX_FLAGS_RELEASE=${MY8033_CXX_FLAGS_RELEASE[*]}")
unset MY8033_CXX_FLAGS_DEBUG
unset MY8033_CXX_FLAGS_RELEASE

# 8.0.32

MY8032_CXX_FLAGS_DEBUG=("${MY8032_34_EXTRA_CXX_FLAGS[@]}" "${CXX_FLAGS_DEBUG[@]}")
MY8032_CXX_FLAGS_RELEASE=("${MY8032_34_EXTRA_CXX_FLAGS[@]}"
                          "${CXX_FLAGS_RELEASE[@]}")
MY8032_EXTRA+=("-DCMAKE_CXX_FLAGS=${MY8032_34_EXTRA_CXX_FLAGS[*]}"
               "-DCMAKE_C_FLAGS_DEBUG=${MY8032_CXX_FLAGS_DEBUG[*]}"
               "-DCMAKE_CXX_FLAGS_DEBUG=${MY8032_CXX_FLAGS_DEBUG[*]}"
               "-DCMAKE_CXX_FLAGS_RELEASE=${MY8032_CXX_FLAGS_RELEASE[*]}")
unset MY8032_34_EXTRA_CXX_FLAGS
unset MY8032_CXX_FLAGS_DEBUG
unset MY8032_CXX_FLAGS_RELEASE

# 8.0.31

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

# 8.0.30

# 8.0.29--8.0.28

MY8028_29_EXTRA_CXX_FLAGS_DEBUG=("-Wno-deprecated-declarations")

# 8.0.29

MY8029_CXX_FLAGS_DEBUG=("${MY8028_29_EXTRA_CXX_FLAGS_DEBUG[@]}"
                        "${CXX_FLAGS_DEBUG[@]}")
MY8029_CXX_FLAGS_RELEASE=("${CXX_FLAGS_RELEASE[@]}")
MY8029_EXTRA=("-DCMAKE_C_FLAGS_DEBUG=${MY8029_CXX_FLAGS_DEBUG[*]}"
              "-DCMAKE_CXX_FLAGS_DEBUG=${MY8029_CXX_FLAGS_DEBUG[*]}"
              "-DCMAKE_CXX_FLAGS_RELEASE=${MY8029_CXX_FLAGS_RELEASE[*]}")
unset MY8029_CXX_FLAGS_DEBUG
unset MY8029_CXX_FLAGS_RELEASE

# 8.0.28--8.0.27

MY8027_28_EXTRA=("-DWITH_FIDO=bundled")

# 8.0.28--8.0.18

MY8018_28_CXX_FLAGS_DEBUG=("${MY8018_28_EXTRA_CXX_FLAGS[@]}"
                           "-Wno-unknown-warning-option"
                           "-Wno-unused-but-set-variable"
                           "-Wno-discarded-qualifiers")

MY8018_28_EXTRA+=("-DWITH_RAPIDJSON=bundled" "-DWITH_LZ4=bundled")

# 8.0.28

FB8028_EXTRA=("-DWITH_UNIT_TESTS=OFF")

MY8028_CXX_FLAGS=("${MY8018_28_EXTRA_CXX_FLAGS[*]}")
MY8028_CXX_FLAGS_DEBUG=("${MY8028_29_EXTRA_CXX_FLAGS_DEBUG[@]}"
                        "${MY8018_28_CXX_FLAGS_DEBUG[*]}"
                        "${CXX_FLAGS_DEBUG[@]}")
MY8028_CXX_FLAGS_RELEASE=("${CXX_FLAGS_RELEASE[@]}")
MY8028_EXTRA=("-DCMAKE_CXX_FLAGS=${MY8028_CXX_FLAGS[*]}"
              "-DCMAKE_C_FLAGS_DEBUG=${MY8028_CXX_FLAGS_DEBUG[*]}"
              "-DCMAKE_CXX_FLAGS_DEBUG=${MY8028_CXX_FLAGS_DEBUG[*]}"
              "-DCMAKE_CXX_FLAGS_RELEASE=${MY8028_CXX_FLAGS_RELEASE[*]}")
unset MY8028_29_EXTRA_CXX_FLAGS_DEBUG
unset MY8028_CXX_FLAGS
unset MY8028_CXX_FLAGS_DEBUG
unset MY8028_CXX_FLAGS_RELEASE

# 8.0.27

MY8027_CXX_FLAGS=("${MY8018_28_EXTRA_CXX_FLAGS[*]}")
MY8027_CXX_FLAGS_DEBUG=("${MY8018_28_EXTRA_CXX_FLAGS[*]}"
                        "${CXX_FLAGS_DEBUG[@]}")
MY8027_CXX_FLAGS_RELEASE=("${MY8018_28_EXTRA_CXX_FLAGS[*]}"
                          "${CXX_FLAGS_RELEASE[@]}")
MY8027_EXTRA=("-DCMAKE_CXX_FLAGS=${MY8027_CXX_FLAGS[*]}"
              "-DCMAKE_C_FLAGS_DEBUG=${MY8027_CXX_FLAGS_DEBUG[*]}"
              "-DCMAKE_CXX_FLAGS_DEBUG=${MY8027_CXX_FLAGS_DEBUG[*]}"
              "-DCMAKE_CXX_FLAGS_RELEASE=${MY8027_CXX_FLAGS_RELEASE[*]}")
unset MY8027_CXX_FLAGS
unset MY8027_CXX_FLAGS_DEBUG
unset MY8027_CXX_FLAGS_RELEASE

# 8.0.26

MY8026_CXX_FLAGS=("${MY8018_28_EXTRA_CXX_FLAGS[*]}")
MY8026_CXX_FLAGS_DEBUG=("${MY8018_28_EXTRA_CXX_FLAGS[*]}"
                        "${CXX_FLAGS_DEBUG[@]}")
MY8026_CXX_FLAGS_RELEASE=("${MY8018_28_EXTRA_CXX_FLAGS[*]}"
                          "${CXX_FLAGS_RELEASE[@]}")
MY8026_EXTRA=("-DCMAKE_CXX_FLAGS=${MY8026_CXX_FLAGS[*]}"
              "-DCMAKE_C_FLAGS_DEBUG=${MY8026_CXX_FLAGS_DEBUG[*]}"
              "-DCMAKE_CXX_FLAGS_DEBUG=${MY8026_CXX_FLAGS_DEBUG[*]}"
              "-DCMAKE_CXX_FLAGS_RELEASE=${MY8026_CXX_FLAGS_RELEASE[*]}")
unset MY8026_28_C_FLAGS_DEBUG

MY8026_EXTRA=("-DENABLE_DOWNLOADS=ON")

# 8.0.18

MY8018_EXTRA_CXX_FLAGS=("-Wno-deprecated-declarations" "-Wno-unused-result"
                        "-Wno-range-loop-construct"
                        "-Wno-non-c-typedef-for-linkage")

MY8018_CXX_FLAGS=("${MY8018_28_EXTRA_CXX_FLAGS[*]}"
                  "${MY8018_EXTRA_CXX_FLAGS[*]}")
MY8026_CXX_FLAGS_DEBUG=("${MY8018_28_EXTRA_CXX_FLAGS[*]}"
                        "${MY8018_28_CXX_FLAGS_DEBUG[*]}"
                        "${MY8018_EXTRA_CXX_FLAGS[*]}" "${CXX_FLAGS_DEBUG[@]}")
MY8018_CXX_FLAGS_RELEASE=("${MY8018_28_EXTRA_CXX_FLAGS[*]}"
                          "${MY8018_EXTRA_CXX_FLAGS[*]}"
                          "${CXX_FLAGS_RELEASE[@]}")
MY8018_EXTRA+=("-DCMAKE_CXX_FLAGS=${MY8026_CXX_FLAGS[*]}"
               "-DCMAKE_C_FLAGS_DEBUG=${MY8026_CXX_FLAGS_DEBUG[*]}"
               "-DCMAKE_CXX_FLAGS_DEBUG=${MY8026_CXX_FLAGS_DEBUG[*]}"
               "-DCMAKE_CXX_FLAGS_RELEASE=${MY8026_CXX_FLAGS_RELEASE[*]}")

unset MY8018_28_CXX_FLAGS_DEBUG
unset MY8018_28_EXTRA_CXX_FLAGS
unset CXX_FLAGS_DEBUG
unset CXX_FLAGS_RELEASE

# Paydirt!

export MY810D=("${MY8D[@]}" "${MY8030_810_EXTRA[@]}")
export MY810=("${MY8R[@]}" "${MY8030_810_EXTRA[@]}")

export MY8034D=("${MY8D[@]}" "${MY8033_34_EXTRA[@]}" "${MY8030_810_EXTRA[@]}"
                "${MY8034_EXTRA[@]}")
export MY8034=("${MY8R[@]}" "${MY8033_34_EXTRA[@]}" "${MY8030_810_EXTRA[@]}"
               "${MY8034_EXTRA[@]}")

export PS8034D=("${MY8D[@]}" "${MY8033_34_EXTRA[@]}" "${MY8030_810_EXTRA[@]}"
                "${MY8034_EXTRA[@]}" "${PS8034_EXTRA[@]}")
unset MY8034_EXTRA
unset PS8034_EXTRA

export MY8033D=("${MY8D[@]}" "${MY8033_34_EXTRA[@]}" "${MY8030_810_EXTRA[@]}"
                "${MY8033_EXTRA[@]}")
export MY8033=("${MY8R[@]}" "${MY8033_34_EXTRA[@]}" "${MY8030_810_EXTRA[@]}"
               "${MY8033_EXTRA[@]}")
unset MY8033_EXTRA
unset MY8033_34_EXTRA

export MY8032D=("${MY8D[@]}" "${MY8032_EXTRA[@]}" "${MY8030_810_EXTRA[@]}")
export MY8032=("${MY8R[@]}" "${MY8032_EXTRA[@]}" "${MY8030_810_EXTRA[@]}")
unset MY8032_EXTRA

export FB8032D=("${MY8032D[@]}" "${FB_COMMON[@]}" "${FB8032_EXTRA[@]}")

export MY8031D=("${MY8D[@]}" "${MY8031_EXTRA[@]}" "${MY8030_810_EXTRA[@]}")
unset MY8031_EXTRA

export MY8030D=("${MY8D[@]}" "${MY8030_810_EXTRA[@]}")
unset MY8030_810_EXTRA

export MY8029D=("${MY8D[@]}" "${MY8029_EXTRA[@]}")

export MY8028=("${MY8R[@]}" "${MY8028_EXTRA[@]}" "${MY8018_28_EXTRA[@]}"
               "${MY8027_28_EXTRA[@]}")
export MY8028D=("${MY8D[@]}" "${MY8028_EXTRA[@]}" "${MY8018_28_EXTRA[@]}"
                "${MY8027_28_EXTRA[@]}")
unset MY8028_29_EXTRA
unset MY8028_EXTRA

export FB8028=("${MY8028[@]}" "${FB_COMMON[@]}" "${FB8028_EXTRA[@]}")
export FB8028D=("${MY8028D[@]}" "${FB_COMMON[@]}" "${FB8028_EXTRA[@]}")
unset FB8028_EXTRA

export MY8027D=("${MY8D[@]}" "${MY8027_EXTRA[@]}" "${MY8018_28_EXTRA[@]}"
                "${MY8027_28_EXTRA[@]}")
unset MY8027_28_EXTRA

export MY8026=("${MY8R[@]}" "${MY8026_EXTRA[@]}" "${MY8018_28_EXTRA[@]}")
export MY8026D=("${MY8D[@]}" "-DDEBUG_EXTNAME=OFF" "${MY8026_EXTRA[@]}"
                "${MY8018_28_EXTRA[@]}")

export MY8018=("${MY8R[@]}" "${MY8018_EXTRA[@]}" "${MY8018_28_EXTRA[@]}")
export MY8018D=("${MY8D[@]}" "${MY8018_EXTRA[@]}" "${MY8018_28_EXTRA[@]}")

unset MY8018_28_EXTRA
unset MY8D
unset MY8R

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
export MY8SAN=("-DWITH_ASAN=ON" "-DWITH_ASAN_SCOPE=ON" "-DWITH_UBSAN=ON")

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
        version_file="../MYSQL_VERSION"
        if [ -d ../rocksdb ]; then
            flavor="facebook"
        else
            version_extra=$(grep MYSQL_VERSION_EXTRA ../MYSQL_VERSION)
            if [ -z $version_extra ]; then
                flavor="mysql"
            else
                flavor="percona"
            fi
        fi
    elif [ -f ../VERSION ]; then
        version_file="../VERSION"
        server_maturity=$(grep SERVER_MATURITY ../VERSION)
        if [ -z $server_maturity ]; then
            flavor="mysql"
        else
            flavor="mariadb"
        fi
    else
        echo "Neither MYSQL_VERSION nor VERSION found"
        return
    fi

    if [ -z $flavor ]; then
        echo "Unknown source tree flavor"
        return
    fi

    major_ver_str=$(grep MYSQL_VERSION_MAJOR $version_file)
    major_ver="${major_ver_str//[^0-9]/}"
    minor_ver_str=$(grep MYSQL_VERSION_MINOR $version_file)
    minor_ver="${minor_ver_str//[^0-9]/}"
    patch_level_str=$(grep MYSQL_VERSION_PATCH $version_file)
    patch_level="${patch_level_str//[^0-9]/}"

    case "$flavor" in
        "mysql")
            echo "Configuring MySQL $major_ver.$minor_ver.$patch_level"
            case "$major_ver.$minor_ver.$patch_level" in
                8.1.0)
                    release_flags=("${MY810[@]}")
                    debug_flags=("${MY810D[@]}")
                    ;;
                8.0.34)
                    release_flags=("${MY8034[@]}")
                    debug_flags=("${MY8034D[@]}")
                    ;;
                8.0.33)
                    release_flags=("${MY8033[@]}")
                    debug_flags=("${MY8033D[@]}")
                    ;;
                8.0.32)
                    release_flags=("${MY8032[@]}")
                    debug_flags=("${MY8032D[@]}")
                    ;;
                8.0.31)
                    release_flags=("${MY8031[@]}")
                    debug_flags=("${MY8031D[@]}")
                    ;;
                8.0.30)
                    release_flags=("${MY8030[@]}")
                    debug_flags=("${MY8030D[@]}")
                    ;;
                8.0.29)
                    release_flags=("${MY8029[@]}")
                    debug_flags=("${MY8029D[@]}")
                    ;;
                8.0.28)
                    release_flags=("${MY8028[@]}")
                    debug_flags=("${MY8028D[@]}")
                    ;;
                8.0.18)
                    release_flags=("${MY8018[@]}")
                    debug_flags=("${MY8018D[@]}")
                    ;;
                *)
                    echo "Unsupported version, please implement"
                    return
                    ;;
            esac
            ;;
        "facebook")
            echo "Configuring Facebook MySQL $major_ver.$minor_ver.$patch_level"
            case "$major_ver.$minor_ver.$patch_level" in
                8.0.32)
                    debug_flags=("${FB8032D[@]}")
                    ;;
                8.0.28)
                    release_flags=("${FB8028[@]}")
                    debug_flags=("${FB8028D[@]}")
                    ;;
                *)
                    echo "Unsupported version, please add"
                    return
                    ;;
            esac
            ;;
        "percona")
            echo "Configuring Percona Server $major_ver.$minor_ver.$patch_level"
            case "$major_ver.$minor_ver.$patch_level" in
                8.0.34)
                    debug_flags=("${PS8034D[@]}")
                    ;;
                *)
                    echo "Unsupported version, please add"
                    return
                    ;;
            esac
            ;;
        "mariadb")
            echo "MariaDB unsupported, please add"
            return
            ;;
        *)
            echo "Unsupported flavor, should have caught sooner"
            return
            ;;
    esac

    build_dir="$(basename "$PWD")"

    case "$build_dir" in
        *san*)
            echo "Using sanitizers"
            sanitizers=1
            debug_flags+=("${MY8SAN[@]}")
            release_flags+=("${MY8SAN[@]}")
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
        *llvm-12*)
            echo "Using LLVM 12"
            debug_flags+=("${MYCLANG12[@]}")
            release_flags+=("${MYCLANG12[@]}")
            ;;
        *llvm*)
            echo "Using LLVM"
            debug_flags+=("${MYCLANG[@]}")
            release_flags+=("${MYCLANG[@]}")
            ;;
    esac

    case "$build_dir" in
        *debug*)
            echo "Debug build"
            cmake -G Ninja .. "${debug_flags[@]}" "$@"
            ;;
        *release*)
            echo "Release build"
            cmake -G Ninja .. "${release_flags[@]}" "$@"
            ;;
    esac
}

mysql_build() {
    mysql_cmake "$@"
    build_dir="$(basename "$PWD")"
    (cd .. && ln -sf "$build_dir/compile_commands.json" .)
    ninja
}

# Workaround P10K going crazy
set +o errexit
set +o nounset
