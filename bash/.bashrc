source ~/.noninteractive_init.bash

# So that Ctrl-s works for forward i-search
stty -ixon

ulimit -c unlimited

export LC_TYPE=C
export LANG=C
export LC_CTYPE=C
export MTR_EMD="--mysqld-env=DYLD_LIBRARY_PATH=/usr/local/lib --mysqld-env=DYLD_FORCE_FLAT_NAMESPACE=1 --mysqld-env=DYLD_INSERT_LIBRARIES=/usr/local/lib/libeatmydata.dylib"
export MYALL="-DBUILD_CONFIG=mysql_release -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
export MY557="-DWITH_ZLIB=system -DWITH_PAM=ON"
export MY55="$MYALL $MY557 -DWITH_EMBEDDED_SERVER=OFF -DWITH_SSL=system -DOPENSSL_ROOT_DIR=/usr/local/opt/openssl/"
export MY567="-DWITH_LIBEVENT=system"
# No point trying to build TokuDB until macOS fixes
export MY568="-DENABLE_DOWNLOADS=ON -DWITH_SSL=/usr/local/opt/openssl/ -DWITHOUT_TOKUDB=ON"
export MY56="$MYALL $MY557 $MY567 $MY568"
# No point trying to build MyRocks until macOS fixes
export MY578="-DDOWNLOAD_BOOST=ON -DWITH_BOOST=~/percona/mysql-boost/ -DWITH_ROCKSDB=OFF -DWITH_KEYRING_VAULT=ON -DWITH_PROTOBUF=system"
export MY57="$MYALL $MY557 $MY567 $MY568 $MY578 -DWITH_CURL=system -DWITH_LZ4=system -DWITH_MECAB=system -DCMAKE_PREFIX_PATH=/usr/local/opt/protobuf@3.1/"
export MY80="$MYALL $MY568 $MY578 -DWITH_AUTHENTICATION_LDAP=ON -DWITH_SYSTEM_LIBS=ON -DWITH_ICU=/usr/local/opt/icu4c -DCMAKE_PREFIX_PATH=/usr/local/opt/protobuf/"
export MY80D="$MY80 -DWITH_DEBUG=ON -DWITH_INNODB_EXTRA_DEBUG=ON"

source $HOME/usr/src/bash-wakatime/bash-wakatime.sh

for script in $HOME/.bash.d/rc/*; do
    source $script
done
