#!/bin/sh
#
# Dear Percona developers –
# if you are still linking to this file on GitHub, I suggest you to use
# https://github.com/laurynas-biveinis/dotfiles/blob/ab2de3f002442880fce8a631ca011cecc33dc774/mysql-work/.bash.d/rc/mysql-work.bash
# – the last Percona version of this file.
#
# Exported environment variables, CMake options:
# MY80, MY80D: Oracle MySQL release & debug build
# MARIA108, MARIA108D: MariaDB 10.8 release and debug build
# FB80, FB80D: MySQL 8.0 Facebook Patch release and debug
#
# Extra CMake options:
# MYSAN: add maximum supported Sanitizer configuration
#
# mysql-test-run options:
# MTR_EMD: MTR options to preload libeatmydata
#
# Works on Linux and macOS.

UNAME_OUT="$(uname -s)"

if [ "$UNAME_OUT" = "Darwin" ]; then
    export MTR_EMD="--mysqld-env=DYLD_LIBRARY_PATH=/usr/local/lib --mysqld-env=DYLD_FORCE_FLAT_NAMESPACE=1 --mysqld-env=DYLD_INSERT_LIBRARIES=/usr/local/lib/libeatmydata.dylib"
    MY_EXTRA="-DWITH_ICU=/usr/local/opt/icu4c"
    MARIA_EXTRA="-DCMAKE_C_FLAGS=\"-isystem /usr/local/include\" -DCMAKE_CXX_FLAGS=\"-isystem /usr/local/include\""
else
    # Linux
    export MTR_EMD="--mysqld-env=LD_PRELOAD=/usr/local/lib/libeatmydata.so"
    MY_EXTRA=""
    MARIA_EXTRA=""
fi

CMAKE_COMMON="-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
CMAKE_RELEASE="-DCMAKE_BUILD_TYPE=RelWithDebugInfo"
CMAKE_DEBUG="-DCMAKE_BUILD_TYPE=Debug"

# -DWITH_MYSQLX=OFF on 8.0.23:
# Undefined symbols for architecture x86_64:
# "_u_cleanup_69", referenced from:
# clean_up(bool) in libsql_main.a(mysqld.cc.o)
MY80_COMMON="-DWITH_DEBUG=ON -DMYSQL_MAINTAINER_MODE=ON -DENABLE_DOWNLOADS=ON -DDOWNLOAD_BOOST=ON -DWITH_BOOST=~/vilniusdb/mysql-boost/ $MY_EXTRA -DWITH_SYSTEM_LIBS=ON -DWITH_RAPIDJSON=bundled -DWITH_LZ4=bundled -DWITH_ROUTER=OFF -DWITH_GROUP_REPLICATION=OFF -DCMAKE_C_FLAGS=-Wno-shadow-field -DCMAKE_CXX_FLAGS=-Wno-shadow-field -DCMAKE_C_FLAGS_DEBUG=-Wno-shadow-field -DCMAKE_CXX_FLAGS_DEBUG=-Wno-shadow-field"

FB_COMMON="-DMYSQL_GITHASH=0 -DMYSQL_GITDATE=2100-02-29 -DROCKSDB_GITHASH=0 -DROCKSDB_GITDATE=2100-02-29"

MARIA_COMMON="-DPLUGIN_MROONGA=NO -DPLUGIN_CONNECT=NO"

export MY80="$CMAKE_COMMON $CMAKE_RELEASE $MY80_COMMON"
export MY80D="$CMAKE_COMMON $CMAKE_DEBUG -DWITH_DEBUG=ON -DWITH_INNODB_EXTRA_DEBUG=ON -DDEBUG_EXTNAME=OFF $MY80_COMMON"

export MY80SAN="-DWITH_ASAN=ON -DWITH_ASAN_SCOPE=ON -DWITH_UBSAN=ON"

export FB80="$MY80 $FB_COMMON"
export FB80D="$MY80 $FB_COMMON"

export MARIA108="$CMAKE_COMMON $CMAKE_RELEASE $MARIA_COMMON $MARIA_EXTRA"
export MARIA108D="$CMAKE_COMMON $CMAKE_DEBUG $MARIA_COMMON $MARIA_EXTRA"

unset UNAME_OUT
unset MY_EXTRA
unset MARIA_EXTRA
unset CMAKE_COMMON
unset CMAKE_RELEASE
unset CMAKE_DEBUG
unset MY80_COMMON
unset MARIA_COMMON
unset FB_COMMON
