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
     protobuf-devel libfido2-devel protobuf-lite-devel libtirpc-devel -y
