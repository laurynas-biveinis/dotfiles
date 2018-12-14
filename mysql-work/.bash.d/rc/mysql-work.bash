#!/bin/bash

UNAME_OUT="$(uname -s)"

if [ "$UNAME_OUT" == "Darwin" ]; then
    export MTR_EMD="--mysqld-env=DYLD_LIBRARY_PATH=/usr/local/lib --mysqld-env=DYLD_FORCE_FLAT_NAMESPACE=1 --mysqld-env=DYLD_INSERT_LIBRARIES=/usr/local/lib/libeatmydata.dylib"
    MY55_SSL="-DWITH_SSL=system -DOPENSSL_ROOT_DIR=/usr/local/opt/openssl/"
    MY568_SSL="-DWITH_SSL=/usr/local/opt/openssl/"
    # No point trying to build TokuDB until macOS fixes
    MY568_EXTRA="-DWITHOUT_TOKUDB=ON"
    # No point trying to build MyRocks until macOS fixes
    MY578_EXTRA="-DWITH_ROCKSDB=OFF"
    MY57_EXTRA="-DCMAKE_PREFIX_PATH=/usr/local/opt/protobuf@3.1/"
    MY80_EXTRA="-DCMAKE_PREFIX_PATH=/usr/local/opt/protobuf/ -DWITH_ICU=/usr/local/opt/icu4c"
else
    # Linux
    export LD_EMD="LD_PRELOAD=/usr/local/lib/libeatmydata.so"
    MY55_SSL="-DWITH_SSL=system"
    MY568_SSL="$MY55_SSL"
    MY568_EXTRA=""
    MY578_EXTRA=""
    MY57_EXTRA=""
    MY80_EXTRA=""
fi

MYALL="-DBUILD_CONFIG=mysql_release -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DWITH_PAM=ON"
MY557="-DWITH_ZLIB=system"
export MY55="$MYALL $MY557 $MY55_SSL"
MY567="-DWITH_LIBEVENT=system"
MY568="-DENABLE_DOWNLOADS=ON $MY568_SSL $MY568_EXTRA"

export MY56="$MYALL $MY557 $MY567 $MY568"
MY578="-DDOWNLOAD_BOOST=ON -DWITH_BOOST=~/percona/mysql-boost/ $MY578_EXTRA -DWITH_KEYRING_VAULT=ON"
export MY56D="$MY56 -DWITH_DEBUG=ON"

export MY57="$MYALL $MY557 $MY567 $MY568 $MY578 -DWITH_CURL=system -DWITH_LZ4=system -DWITH_MECAB=system -DWITH_PROTOBUF=system $MY57_EXTRA"
export MY57D="$MY57 -DWITH_DEBUG=ON"

export MY80="$MYALL $MY568 $MY578 -DWITH_AUTHENTICATION_LDAP=ON -DWITH_SYSTEM_LIBS=ON $MY80_EXTRA"
export MY80D="$MY80 -DWITH_DEBUG=ON -DWITH_INNODB_EXTRA_DEBUG=ON"

export MY5SAN="-DWITH_ASAN=ON -DWITH_ASAN_SCOPE=ON"
export MYSAN="$MY5SAN -DWITH_UBSAN=ON"

export MYGCC7="-DCMAKE_C_COMPILER=gcc-7 -DCMAKE_CXX_COMPILER=g++-7"

unset UNAME_OUT
unset MY55_SSL
unset MY568_SSL
unset MY568_EXTRA
unset MY578_EXTRA
unset MY57_EXTRA
unset MY80_EXTRA
unset MYALL
unset MY557
unset MY567
unset MY568
unset MY578

alias rmcores="rm -rf /cores/*"
