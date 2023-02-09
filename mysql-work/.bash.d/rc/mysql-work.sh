#!/bin/sh
#
# Exported environment variables, CMake options:
# MY80xy, MY80xyD: Oracle MySQL release & debug build, for xy patchlevel version
# MARIA108, MARIA108D: MariaDB 10.8 release and debug build
# FB80, FB80D: MySQL 8.0 Facebook Patch release and debug
#
# Extra CMake options:
# MY_CLANG: set compiler to clang for CMake
# MYSAN: add maximum supported Sanitizer configuration
#
# mysql-test-run options:
# MTR_EMD: MTR options to preload libeatmydata
#
# Works on Linux and macOS.

UNAME_OUT="$(uname -s)"

# Common building blocks

if [ "$UNAME_OUT" = "Darwin" ]; then
    if [ "$(arch)" = "arm64" ]; then
        BREW="/opt/homebrew/opt"
        MY8028_OS_EXTRA="-DWITH_SSL=$BREW/openssl@1.1"
        MY8030_OS_EXTRA="-DWITH_DEVELOPER_ENTITLEMENTS=ON"
    else
        MY8028_OS_EXTRA=""
        MY8030_OS_EXTRA=""
    fi
    MY8028_OS_EXTRA="$MY8028_OS_EXTRA -DWITH_ICU=$BREW/icu4c"
    export MYCLANG12="-DCMAKE_C_COMPILER=$BREW/llvm@12/bin/clang-12 \
 -DCMAKE_CXX_COMPILER=$BREW/llvm@12/bin/clang++"
    MARIA_EXTRA="-DCMAKE_C_FLAGS=\"-isystem /usr/local/include\" \
-DCMAKE_CXX_FLAGS=\"-isystem /usr/local/include\""
    FB_EXTRA=""
    MY8028_EXTRA_CXX_FLAGS="-Wno-shadow-field"
    MY8031_EXTRA_CXX_FLAGS="-Wno-deprecated-declarations"
else
    MARIA_EXTRA=""
    FB_EXTRA="-DWITH_ZSTD=bundled -DWITH_PROTOBUF=bundled"
    MY8028_EXTRA_CXX_FLAGS="-Wno-unused-label"
    MY8031_EXTRA_CXX_FLAGS=""
    MY8028_OS_EXTRA=""
    MY8030_OS_EXTRA=""
    export MYCLANG12="-DCMAKE_C_COMPILER=clang-12 \
-DCMAKE_CXX_COMPILER=clang++-12"
fi

CMAKE_COMMON="-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
CMAKE_RELEASE="$CMAKE_COMMON -DBUILD_CONFIG=mysql_release \
-DCMAKE_BUILD_TYPE=Release"
CMAKE_DEBUG="$CMAKE_COMMON -DCMAKE_BUILD_TYPE=Debug -DWITH_DEBUG=ON"
unset CMAKE_COMMON

MY80="-DMYSQL_MAINTAINER_MODE=ON -DDOWNLOAD_BOOST=ON \
-DWITH_BOOST=~/vilniusdb/mysql-boost/ -DWITH_SYSTEM_LIBS=ON -DWITH_ROUTER=OFF \
-DWITH_UNIT_TESTS=OFF"

# Workaround Facebook tooling incompatibility with git worktrees
FB_COMMON="-DMYSQL_GITHASH=0 -DMYSQL_GITDATE=2100-02-29 -DROCKSDB_GITHASH=0 \
-DROCKSDB_GITDATE=2100-02-29 $FB_EXTRA"
unset FB_EXTRA

MARIA_COMMON="-DPLUGIN_MROONGA=NO -DPLUGIN_CONNECT=NO $MARIA_EXTRA"
unset MARIA_EXTRA

# Version-specific building blocks, descending order

MY8031_EXTRA="-DCMAKE_CXX_FLAGS=$MY8031_EXTRA_CXX_FLAGS \
-DCMAKE_C_FLAGS_DEBUG='$MY8031_EXTRA_CXX_FLAGS -g' \
-DCMAKE_CXX_FLAGS_DEBUG='$MY8031_EXTRA_CXX_FLAGS -g' \
-DCMAKE_CXX_FLAGS_RELEASE='$MY8031_EXTRA_CXX_FLAGS -O2 -g -DNDEBUG'"
unset MY8031_EXTRA_CXX_FLAGS

MY8030_EXTRA="$MY8030_OS_EXTRA"
unset MY8030_OS_EXTRA

MY8028_29_EXTRA="-DCMAKE_C_FLAGS_DEBUG='-Wno-deprecated-declarations -g' \
-DCMAKE_CXX_FLAGS_DEBUG='-Wno-deprecated-declarations -g'"

MY8027_28_EXTRA="-DWITH_FIDO=bundled"

MY8028_EXTRA="-DWITH_RAPIDJSON=bundled -DWITH_LZ4=bundled $MY8028_OS_EXTRA \
-DCMAKE_CXX_FLAGS=$MY8028_EXTRA_CXX_FLAGS \
-DCMAKE_C_FLAGS_DEBUG='$MY8028_EXTRA_CXX_FLAGS -Wno-unknown-warning-option \
-Wno-unused-but-set-variable -Wno-discarded-qualifiers' \
-DCMAKE_CXX_FLAGS_DEBUG='$MY8028_EXTRA_CXX_FLAGS -Wno-unknown-warning-option \
-Wno-unused-but-set-variable' \
-DCMAKE_CXX_FLAGS_RELEASE='$MY8028_EXTRA_CXX_FLAGS -O2 -g -DNDEBUG'"

unset MY8028_EXTRA_CXX_FLAGS

MY8027_EXTRA="$MY8028_EXTRA"

MY8026_EXTRA="-DENABLE_DOWNLOADS=ON $MY8027_EXTRA"

# Paydirt!

export MY8032D="$CMAKE_DEBUG $MY80"
export MY8032="$CMAKE_RELEASE $MY80"

export MY8031D="$CMAKE_DEBUG $MY80 $MY8031_EXTRA"
unset MY8031_EXTRA

export MY8030D="$CMAKE_DEBUG $MY80 $MY8030_EXTRA"
unset MY8030_EXTRA

export MY8029D="$CMAKE_DEBUG $MY80 $MY8028_29_EXTRA"

export MY8028="$CMAKE_RELEASE $MY80 $MY8028_EXTRA $MY8027_28_EXTRA \
$MY8028_29_EXTRA"
export MY8028D="$CMAKE_DEBUG $MY80 $MY8028_EXTRA $MY8027_28_EXTRA \
$MY8028_29_EXTRA"
unset MY8028_29_EXTRA
unset MY8028_EXTRA

export FB8028="$MY8028 $FB_COMMON"
export FB8028D="$MY8028D $FB_COMMON"

export MY8027D="$CMAKE_DEBUG $MY80 $MY8027_EXTRA $MY8027_28_EXTRA"
unset MY8027_EXTRA
unset MY8027_28_EXTRA

export MY8026="$CMAKE_RELEASE $MY80 $MY8026_EXTRA"
export MY8026D="$CMAKE_DEBUG -DDEBUG_EXTNAME=OFF $MY80 $MY8026_EXTRA"
unset MY8026_EXTRA
unset MY80

export FB8026="$MY8026 $FB_COMMON"
export FB8026D="$MY8026D $FB_COMMON"

export MARIA108="$CMAKE_RELEASE $MARIA_COMMON"
export MARIA108D="$CMAKE_DEBUG $MARIA_COMMON"

unset CMAKE_RELEASE
unset CMAKE_DEBUG

# Addons, environment helpers
export MYCLANG="-DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++"
export MYCLANG13="-DCMAKE_C_COMPILER=clang-13 -DCMAKE_CXX_COMPILER=clang++-13"
export MY80SAN="-DWITH_ASAN=ON -DWITH_ASAN_SCOPE=ON -DWITH_UBSAN=ON"

if [ "$UNAME_OUT" = "Darwin" ]; then
    export MTR_EMD="--mysqld-env=DYLD_LIBRARY_PATH=$BREW/libeatmydata/lib/ \
--mysqld-env=DYLD_FORCE_FLAT_NAMESPACE=1 \
--mysqld-env=DYLD_INSERT_LIBRARIES=$BREW/libeatmydata/lib/libeatmydata.dylib"
    unset BREW
else
    export MTR_EMD="--mysqld-env=LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libeatmydata.so"
fi

mtr_emd() {
    mtr_emd_tmp_dir=$(mktemp -d /tmp/mtr-XXXX)
    # The correct way would be to use arrays, but those don't exist in POSIX sh.
    # shellcheck disable=SC2086
    ./mtr $MTR_EMD --tmpdir=$mtr_emd_tmp_dir "$@"
}

rm_tmp_mtr() {
    rm -rf /tmp/mtr-*
}

# shellcheck disable=SC2120
mysql_cmake() {
    if [ -f ../MYSQL_VERSION ]; then
        major_ver_str=$(grep MYSQL_VERSION_MAJOR ../MYSQL_VERSION)
        major_ver=$(echo "$major_ver_str" | sed 's/[^0-9]//g')
        if [ "$major_ver" != 8 ]; then
            echo "Only MySQL version 8 is supported"
            return
        fi
        patch_level_str=$(grep MYSQL_VERSION_PATCH ../MYSQL_VERSION)
        patch_level=$(echo "$patch_level_str" | sed 's/[^0-9]//g')

        echo "Configuring MySQL $major_ver.0.$patch_level"
        case $patch_level in
            32)
                release_flags=$MY8032
                debug_flags=$MY8032D
                ;;
        esac
    elif [ -f ../VERSION ]; then
        echo "Configuring MariaDB"
    else
        echo "Neither MariaDB nor MySQL source tree"
        return
    fi

    build_dir="$(basename "$PWD")"
    case "$build_dir" in
        *debug*)
            # The correct way would be to use arrays, but those don't exist in POSIX sh.
            # shellcheck disable=SC2086
            eval cmake .. $debug_flags "$@"
            ;;
        *release*)
            # shellcheck disable=SC2086
            eval cmake .. $release_flags "$@"
            ;;
    esac
}

mysql_build() {
    mysql_cmake
    make -j "$MAKE_J"
}

unset UNAME_OUT
