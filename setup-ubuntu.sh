#!/bin/sh

set -eu

# Not actually meant for running as an invoked script, but rather as a sequence
# of steps to consult and copy and paste into the terminal, and to do some
# unscripted steps manually.

sudo apt update
sudo apt dist-upgrade

# If minimized:
sudo apt install locales
sudo locale-gen en_US.utf8
sudo update-locale

sudo apt install net-tools

# Common
# Not needed in the minimal install:
# missing in 25.10: tldr
sudo apt install manpages-dev binutils libc6-dev software-properties-common \
	unattended-upgrades screen psmisc man-db htop jq strace lsof stow \
	git-doc diffutils-doc perl-doc make build-essential gdb binutils-doc cpp \
	g++ gcc liblsan0 libtsan0 libubsan1 cpp-doc gcc-doc autoconf automake \
	libtool flex bison glibc-doc make-doc autoconf-archive gnu-standards \
	autoconf-doc gettext bison-doc flex-doc gdb-doc gettext-doc libtool-doc \
	m4-doc cmake cmake-doc diffstat unzip pinentry-doc zip colordiff valgrind \
	linux-tools-generic libjemalloc2 python3-scipy python3-pytest \
	python-scipy-doc fzf fd-find clang moreutils zsh zsh-doc libjemalloc-dev \
	bzip2-doc hexyl ripgrep icu-doc ncurses-doc pkg-config lcov cpufrequtils \
	libboost-dev libboost-doc cppcheck iwyu cpuset python3-pip ninja-build \
	cmake-format clang-tidy neovim lynis shellcheck bat cpplint duf eza \
	python3-pandas pipx
# Compiler versions
# Absent in 24.10+
sudo apt install g++-10 libgfortran-10-dev
sudo apt install g++-11 libgfortran-11-dev g++-12 libgfortran-12-dev \
	g++-13 libgfortran-13-dev g++-14 libgfortran-14-dev cpp-14-doc gcc-14-doc \
	libstdc++-14-doc
# Absent in 24.04+
sudo apt install clang-12 clang-12-doc clang-tools-12 lld-12 llvm-12 \
	llvm-12-doc llvm-12-tools
# Absent in 24.10+
sudo apt install clang-13 clang-13-doc clang-tools-13 lld-13 llvm-13 \
	llvm-13-doc llvm-13-tools
# Absent in 25.10
sudo apt install clang-15 clang-15-doc clang-tools-15 lld-15 llvm-15 \
	llvm-15-doc llvm-15-tools \
	clang-16 clang-16-doc clang-tools-16 lld-16 llvm-16 \
	llvm-16-doc llvm-16-tools
sudo apt install clang-14 clang-14-doc clang-tools-14 lld-14 llvm-14 \
	llvm-14-doc llvm-14-tools \
	clang-17 clang-17-doc clang-tools-17 lld-17 llvm-17 \
	llvm-17-doc llvm-17-tools \
	clang-18 clang-18-doc clang-format-18 clang-tidy-18 \
	clang-tools-18 lld-18 llvm-18 llvm-18-doc llvm-18-tools \
	clang-19 clang-19-doc clang-format-19 clang-tidy-19 \
	clang-tools-19 clangd-19 lld-19 llvm-19 llvm-19-doc llvm-19-tools
# Absent in 24.10:
sudo apt install libasan5 libstdc++-doc libgfortran-dev
# DeepState (x86_64 only)
sudo apt install libc6-dev-i386
# Not found on AWS EC2
sudo apt install acpi python3-doc
# Not named fd by default because fdclone (which I don't use) was first
sudo ln -sf /usr/bin/fdfind /usr/local/bin/fd
# So that I can have "gsed" as a GNU Sed on any platform
sudo ln -sf /bin/sed /bin/gsed
# sudo usermod -aG docker laurynas

# For CPU-intensive benchmarks
sudo sh -c "echo -1 > /proc/sys/kernel/perf_event_paranoid"
sudo sysctl -w vm.swappiness=0
sudo sysctl -w kernel.kptr_restrict=0

# kernel.perf_event_paranoid = -1
# kernel.kptr_restrict = 0
# vm.swappiness = 0
sudo nano /etc/sysctl.conf

# x86_64 only:
sudo nano /etc/default/cpufrequtils # GOVERNOR="performance"
sudo /etc/init.d/cpufrequtils restart
# # After each apt upgrade
# # (https://twitter.com/trav_downs/status/1280004737455271936):
# sudo ln -s /usr/lib/debug/lib/x86_64-linux-gnu/* /usr/lib/debug/usr/lib/x86_64-linux-gnu/

# kernel.yama.ptrace_scope = 0
sudo nano /etc/sysctl.d/10-ptrace.conf

sudo sh -c "echo 0 > /proc/sys/kernel/yama/ptrace_scope"
