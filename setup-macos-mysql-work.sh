#!/bin/zsh

brew install ghostscript bison libfido2 doxygen graphviz lz4 libeatmydata \
     rapidjson protobuf@21 llvm@16 openblas llvm@17 llvm@14
brew link protobuf@21
sudo ln -sf /opt/homebrew/opt/gcc/lib/gcc/current/libgfortran.a \
     `xcode-select -p`/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/lib/libgfortran.a
