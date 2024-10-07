#!/bin/zsh

brew install ghostscript bison libfido2 doxygen graphviz lz4 libeatmydata \
     rapidjson protobuf@21 llvm@14 llvm@15 llvm@16 llvm@17 llvm@18 openblas \
     perl mysql-client@8.0

# Set the default clang-format version to what upstreams use. Note that this
# does not affect clangd.
sudo ln -sf /opt/homebrew/opt/llvm@18/bin/clang-format \
     /usr/local/bin/clang-format
brew link protobuf@21

# Needs to be re-executed after every XCode update
sudo ln -sf /opt/homebrew/opt/gcc/lib/gcc/current/libgfortran.a \
     `xcode-select -p`/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/lib/libgfortran.a

# Perl
# TODO(laurynas): does not survive upgrades, do what the Perl formula advises
# with local::lib
/opt/homebrew/opt/perl/bin/cpan DBI
/opt/homebrew/opt/perl/bin/cpan JSON
/opt/homebrew/opt/perl/bin/cpan Expect
# Adjust path only for compilation below. For now I don't need it globally
export PATH="/opt/homebrew/opt/mysql-client/bin:$PATH"
# Will fail
# https://stackoverflow.com/questions/53277884/installation-of-dbdmysql-fails-with-symbol-not-found-on-osx-mojave
/opt/homebrew/opt/perl/bin/cpan DBD::mysql
# Go to ~/.cpan/build/DBD-mysql-<latest> and do:
perl Makefile.PL \
     --libs="-L/opt/homebrew/opt/mysql-client/lib -L/opt/homebrew/lib -lmysqlclient -lz -lzstd -lssl -lcrypto -lresolv"
sudo make install
