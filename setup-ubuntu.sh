#!/bin/sh

# Not actually meant for running as an invoked script, but rather as a sequence
# of steps to consult and copy and paste into the terminal, and to do some
# unscripted steps manually.

sudo apt-get install locales
sudo locale-gen en_US.utf8
sudo update-locale

sudo apt-get install net-tools

# On the first SSH login allow Amazon Q to install!

# Common
sudo apt-get update
sudo apt-get dist-upgrade
sudo apt-get install stow tldr git-doc diffutils-doc perl-doc make \
	build-essential gdb manpages-dev binutils binutils-doc cpp g++ gcc \
	libasan5 liblsan0 libtsan0 libubsan1 libc6-dev cpp-doc gcc-doc autoconf \
	automake libtool flex bison glibc-doc make-doc autoconf-archive \
	gnu-standards autoconf-doc gettext bison-doc flex-doc gdb-doc \
	gettext-doc libtool-doc m4-doc cmake cmake-doc diffstat unzip \
	pinentry-doc zip software-properties-common colordiff valgrind \
	linux-tools-generic libjemalloc2 python3-scipy python3-pytest \
	python-scipy-doc unattended-upgrades screen fzf fd-find clang \
	moreutils psmisc zsh zsh-doc libjemalloc-dev bzip2-doc hexyl ripgrep \
	icu-doc ncurses-doc pkg-config lcov cpufrequtils libboost-dev \
	libboost-doc cppcheck iwyu cpuset python3-pip ninja-build cmake-format \
	clang-tidy neovim man-db lynis shellcheck htop jq bat cpplint duf eza \
	python3-pandas pipx strace lsof libstdc++-doc pipx libgfortran-dev
# Compiler versions
sudo apt-get install g++-10 gcc-10-doc libgfortran-10-dev
sudo apt-get install g++-11 gcc-11-doc libgfortran-11-dev
sudo apt-get install g++-12 cpp-12-doc gcc-12-doc libgfortran-12-dev
# Default in 24.04
sudo apt-get install libstdc++-13-doc
# Absent in 24.04
sudo apt-get install clang-12 clang-12-doc clang-tools-12 lld-12 llvm-12 \
	llvm-12-doc llvm-12-tools
sudo apt-get install clang-14 clang-14-doc clang-tools-14 lld-14 llvm-14 \
	llvm-14-doc llvm-14-tools
sudo apt-get install clang-15 clang-15-doc clang-tools-15 lld-15 llvm-15 \
	llvm-15-doc llvm-15-tools
sudo apt-get install clang-16 clang-16-doc clang-tools-16 lld-16 llvm-16 \
	llvm-16-doc llvm-16-tools
sudo apt-get install clang-17 clang-17-doc clang-tools-17 lld-17 llvm-17 \
	llvm-17-doc llvm-17-tools
sudo apt-get install clang-18 clang-18-doc clang-format-18 clang-tidy-18 \
	clang-tools-18 clangd-18 lld-18 llvm-18 llvm-18-doc llvm-18-tools
# DeepState (x86_64 only)
sudo apt-get install libc6-dev-i386
# Not found on AWS EC2
sudo apt-get install acpi python3-doc
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
