#!/bin/zsh

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup component add rust-src
rustup component add rust-analyzer
rustup component add llvm-tools-preview
cargo install grcov cargo-machete cargo-audit

brew install cargo-nextest
