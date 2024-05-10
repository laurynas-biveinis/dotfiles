#!/bin/sh

sudovi # enable wheel
adduser laurynas
passwd laurynas
usermod -aG wheel laurynas
yum install epel-release openssh-clients -y # scp now works
# As user:
sudo yum install \
	http://opensource.wandisco.com/centos/6/git/x86_64/wandisco-git-release-6-1.noarch.rpm \
	-y
sudo yum install git -y
sudo yum install stow stow-doc python-pip gdb man-pages man-pages-overrides \
	binutils cpp gcc-c++ autoconf automake libtool flex bison gettext cmake \
	diffstat unzip zip colordiff wget -y
# Work
sudo yum install libev-devel openssl-devel openssl-static lua-devel lua-static \
	zlib-devel readline-devel java-1.8.0-openjdk java-1.8.0-openjdk-devel clang \
	libcurl-devel scl-utils cmake3 devtoolset-2-binutils devtoolset-2-gcc-c++ \
	openldap-devel -y
