#!/bin/zsh

brew install ghostscript bison libfido2 doxygen graphviz lz4 libeatmydata \
     rapidjson protobuf@21 llvm@16 openblas llvm@17 llvm@14
# Set the default clang-format version to what upstreams use. Note that this
# does not affect clangd.
sudo ln -sf /opt/homebrew/opt/llvm@18/bin/clang-format \
     /usr/local/bin/clang-format
brew link protobuf@21
# Needs to be re-executed after every XCode update
sudo ln -sf /opt/homebrew/opt/gcc/lib/gcc/current/libgfortran.a \
     `xcode-select -p`/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/lib/libgfortran.a
