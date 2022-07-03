#!/bin/sh

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
     psmisc zsh zsh-doc libjemalloc-dev bzip2-doc libstdc++-11-doc \
     valgrind-dbg hexyl ripgrep icu-doc ncurses-doc pkg-config lcov \
     cpufrequtils libboost-dev libboost-doc cppcheck iwyu clang-12 clang-12-doc \
     llvm-12-doc lld-12 clang-11 clang-11-doc llvm-11-doc lld-11
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
echo 'deb http://apt.llvm.org/impish/ llvm-toolchain-impish-13 main' \
    | sudo tee -a /etc/apt/sources.list
sudo apt-get install clang-13 libomp5-13 llvm-13 lld-13 clang-tools-13 \
     clang-tidy-13 clang-13-doc llvm-13-doc clang-format-13
pip install pandas
# MySQL development specific
sudo apt-get install ccache rapidjson-dev ncdu libaio-dev libssl-dev \
     libreadline-dev readline-doc liblz4-dev libre2-dev libicu-dev zlib1g-dev \
     libevent-dev libcurl4-gnutls-dev libcurl4-doc libpam0g-dev libtirpc-dev \
     libprotobuf-dev libldap2-dev libsasl2-dev libnuma-dev mecab libprotoc-dev \
     doxygen doxygen-doc graphviz graphviz-doc libedit-dev libgcrypt20-dev \
     libfido2-dev libssl-doc rapidjson-doc libeatmydata1 libudev-dev
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

# Edit `/etc/sysctl.d/10-ptrace.conf` for `kernel.yama.ptrace_scope = 0`

sudo sh -c "echo 0 > /proc/sys/kernel/yama/ptrace_scope"
