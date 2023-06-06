#!/bin/sh

dnf install net-tools -y

adduser laurynas
passwd laurynas
usermod -aG wheel laurynas

sudo dnf install epel-release -y
sudo dnf config-manager --set-enabled crb

sudo dnf install git stow stow-doc python-pip gdb man-pages binutils cpp \
     gcc-c++ autoconf automake libtool flex bison gettext cmake diffstat unzip \
     zip colordiff wget zlib-devel libzstd-devel util-linux-user zsh \
     openssl-devel cyrus-sasl-devel openldap-devel libedit-devel \
     libevent-devel libicu-devel libcurl-devel rpcgen clang lz4-devel \
     protobuf-devel libfido2-devel protobuf-lite-devel libtirpc-devel \
     gcc-toolset-12-libasan-devel ncdu lynis shellcheck ghostscript htop \
     fd-find jq cppcheck bat doxygen graphviz ripgrep libeatmydata exa \
     ninja-build recode golang perl-Memoize perl-English perl-Time \
     perl-Time-HiRes perl-Sys-Hostname -y

# TODO(laurynas): fix paths
go install github.com/rhysd/actionlint/cmd/actionlint@latest

# TODO(laurynas): fix paths
pip install cpplint

# TODO(laurynas): fzf
# TODO(laurynas): include-what-you-use
# TODO(laurynas): infer
# TODO(laurynas): creduce
# TODO(laurynas): circleci
# TODO(laurynas): duf
# TODO(laurynas): delta
