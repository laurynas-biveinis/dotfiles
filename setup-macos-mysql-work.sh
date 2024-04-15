#!/bin/zsh

brew install ghostscript bison libfido2 doxygen graphviz lz4 libeatmydata \
     rapidjson protobuf@21 llvm@16 openblas
brew link protobuf@21
sudo ln -sf /opt/homebrew/opt/llvm@16/bin/clang-format \
     /usr/local/bin/clang-format
