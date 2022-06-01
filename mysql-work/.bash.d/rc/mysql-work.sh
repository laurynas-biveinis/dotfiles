#!/bin/sh
#
# Dear Percona developers –
# if you are still linking to this file on GitHub, I suggest you to use
# https://github.com/laurynas-biveinis/dotfiles/blob/ab2de3f002442880fce8a631ca011cecc33dc774/mysql-work/.bash.d/rc/mysql-work.bash
# – the last Percona version of this file.
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
    MARIA_EXTRA="-DCMAKE_C_FLAGS=\"-isystem /usr/local/include\" \
-DCMAKE_CXX_FLAGS=\"-isystem /usr/local/include\""
    FB_EXTRA=""
    MY8028_EXTRA_CXX_FLAGS="-Wno-shadow-field"
else
    MARIA_EXTRA=""
    FB_EXTRA="-DWITH_ZSTD=bundled -DWITH_PROTOBUF=bundled"
    MY8028_EXTRA_CXX_FLAGS=""
fi

CMAKE_COMMON="-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
CMAKE_RELEASE="$CMAKE_COMMON -DCMAKE_BUILD_TYPE=RelWithDebugInfo"
CMAKE_DEBUG="$CMAKE_COMMON -DCMAKE_BUILD_TYPE=Debug -DWITH_DEBUG=ON"
unset CMAKE_COMMON

# -DWITH_MYSQLX=OFF on 8.0.23-8.0.29:
# Undefined symbols for architecture x86_64:
# "_u_cleanup_69", referenced from:
# clean_up(bool) in libsql_main.a(mysqld.cc.o)
MY80="-DMYSQL_MAINTAINER_MODE=ON -DDOWNLOAD_BOOST=ON \
-DWITH_BOOST=~/vilniusdb/mysql-boost/ -DWITH_SYSTEM_LIBS=ON -DWITH_ROUTER=OFF \
-DWITH_GROUP_REPLICATION=OFF -DWITH_UNIT_TESTS=OFF"

# Workaround Facebook tooling incompatibility with git worktrees
FB_COMMON="-DMYSQL_GITHASH=0 -DMYSQL_GITDATE=2100-02-29 -DROCKSDB_GITHASH=0 \
-DROCKSDB_GITDATE=2100-02-29 $FB_EXTRA"
unset FB_EXTRA

MARIA_COMMON="-DPLUGIN_MROONGA=NO -DPLUGIN_CONNECT=NO $MARIA_EXTRA"
unset MARIA_EXTRA

# Version-specific building blocks, descending order

MY8028_EXTRA=\
"-DWITH_RAPIDJSON=bundled -DWITH_LZ4=bundled \
-DCMAKE_CXX_FLAGS=$MY8028_EXTRA_CXX_FLAGS \
-DCMAKE_C_FLAGS_DEBUG='-Wno-deprecated-declarations \
-Wno-unused-but-set-variable -g' \
-DCMAKE_CXX_FLAGS_DEBUG='$MY8028_EXTRA_CXX_FLAGS -Wno-deprecated-declarations \
-Wno-unused-but-set-variable -g'"

unset MY8028_EXTRA_CXX_FLAGS

if [ "$UNAME_OUT" = "Darwin" ]; then
    MY8027_EXTRA="-DWITH_ICU=/usr/local/opt/icu4c $MY8028_EXTRA"
else
    MY8027_EXTRA="$MY8028_EXTRA"
fi

MY8026_EXTRA="-DENABLE_DOWNLOADS=ON $MY8027_EXTRA"

# Paydirt!

export MY8029D="$CMAKE_DEBUG $MY80"
unset MY8028_EXTRA

export MY8027D="$CMAKE_DEBUG $MY80 $MY8027_EXTRA -DWITH_FIDO=bundled"
unset MY8027_EXTRA

export MY8026="$CMAKE_RELEASE $MY80 $MY8026_EXTRA"
export MY8026D="$CMAKE_DEBUG -DDEBUG_EXTNAME=OFF $MY80 $MY8026_EXTRA"
unset MY8026_EXTRA
unset MY80

export FB80="$MY8026 $FB_COMMON"
export FB80D="$MY8026D $FB_COMMON"

export MARIA108="$CMAKE_RELEASE $MARIA_COMMON"
export MARIA108D="$CMAKE_DEBUG $MARIA_COMMON"

unset CMAKE_RELEASE
unset CMAKE_DEBUG

# Addons, environment helpers
export MY_CLANG="-DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++"
export MY80SAN="-DWITH_ASAN=ON -DWITH_ASAN_SCOPE=ON -DWITH_UBSAN=ON"

if [ "$UNAME_OUT" = "Darwin" ]; then
    export MTR_EMD="--mysqld-env=DYLD_LIBRARY_PATH=/usr/local/lib \
--mysqld-env=DYLD_FORCE_FLAT_NAMESPACE=1 \
--mysqld-env=DYLD_INSERT_LIBRARIES=/usr/local/lib/libeatmydata.dylib"
else
    export MTR_EMD="--mysqld-env=LD_PRELOAD=/usr/local/lib/libeatmydata.so"
fi

unset UNAME_OUT
