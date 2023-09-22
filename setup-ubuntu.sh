#!/bin/sh

# Not actually meant for running as an invoked script, but rather as a sequence
# of steps to consult and copy and paste into the terminal, and to do some
# unscripted steps manually.

sudo apt-get install net-tools

# Common
sudo apt-get update
sudo apt-get dist-upgrade
sudo apt-get install stow tldr git-doc diffutils-doc perl-doc make \
     build-essential gdb manpages-dev binutils binutils-doc cpp g++ gcc \
     libasan5 liblsan0 libtsan0 libubsan1 libc6-dev cpp-doc gcc-doc autoconf \
     automake libtool flex bison glibc-doc make-doc autoconf-archive \
     gnu-standards autoconf-doc gettext bison-doc flex-doc gdb-doc gettext-doc \
     libtool-doc m4-doc cmake cmake-doc diffstat unzip pinentry-doc zip \
     software-properties-common colordiff valgrind linux-tools-generic \
     libjemalloc2 python3-scipy python-numpy-doc python3-pytest \
     python-scipy-doc unattended-upgrades screen fzf fd-find clang moreutils \
     psmisc zsh zsh-doc libjemalloc-dev bzip2-doc valgrind-dbg hexyl ripgrep \
     icu-doc ncurses-doc pkg-config lcov cpufrequtils libboost-dev \
     libboost-doc cppcheck iwyu clang-12 clang-12-doc llvm-12-doc lld-12 \
     clang-11 clang-11-doc llvm-11-doc lld-11 cpuset python3-pip g++-12 \
     gcc-12-doc libstdc++-12-doc g++-10 gcc-10-doc clang-14-doc ninja-build \
     cmake-format clang-tidy neovim man-db lynis shellcheck htop jq bat \
     cpplint duf exa
# DeepState
sudo apt-get install libc6-dev-i386
# Not found on AWS EC2
sudo apt-get install acpi python3-pip python3-doc
# Not named fd by default because fdclone (which I don't use) was first
sudo ln -sf /usr/bin/fdfind /usr/local/bin/fd
# So that I can have "gsed" as a GNU Sed on any platform
sudo ln -sf /bin/sed /bin/gsed
# sudo usermod -aG docker laurynas
# LLVM
wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | sudo apt-key add -
echo 'deb http://apt.llvm.org/jammy/ llvm-toolchain-jammy-17 main' \
    | sudo tee -a /etc/apt/sources.list
sudo apt-get update
sudo apt-get install clang-17 libomp5-17 llvm-17 lld-17 clang-tools-17 \
     clang-tidy-17 clang-17-doc llvm-17-doc clang-format-17
pip install pandas
# MySQL development specific
sudo apt-get install ccache rapidjson-dev ncdu libaio-dev libssl-dev \
     libreadline-dev readline-doc liblz4-dev libre2-dev libicu-dev zlib1g-dev \
     libevent-dev libcurl4-gnutls-dev libcurl4-doc libpam0g-dev libtirpc-dev \
     libprotobuf-dev libldap2-dev libsasl2-dev libnuma-dev mecab libprotoc-dev \
     doxygen doxygen-doc graphviz graphviz-doc libedit-dev libgcrypt20-dev \
     libfido2-dev libssl-doc rapidjson-doc libeatmydata1 libudev-dev \
     libzstd-dev protobuf-compiler libsasl2-dev libsasl2-modules-gssapi-mit \
     libkrb5-dev

# For CPU-intensive benchmarks
sudo sh -c "echo -1 > /proc/sys/kernel/perf_event_paranoid"
sudo sysctl -w vm.swappiness=0
sudo sysctl -w kernel.kptr_restrict=0

# kernel.perf_event_paranoid = -1
# kernel.kptr_restrict = 0
# vm.swappiness = 0
sudo nano /etc/sysctl.conf

sudo nano /etc/default/cpufrequtils # GOVERNOR="performance"
sudo /etc/init.d/cpufrequtils restart
# # After each apt upgrade
# # (https://twitter.com/trav_downs/status/1280004737455271936):
# sudo ln -s /usr/lib/debug/lib/x86_64-linux-gnu/* /usr/lib/debug/usr/lib/x86_64-linux-gnu/

# kernel.yama.ptrace_scope = 0
sudo nano /etc/sysctl.d/10-ptrace.conf

sudo sh -c "echo 0 > /proc/sys/kernel/yama/ptrace_scope"
