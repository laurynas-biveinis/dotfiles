#!/bin/bash

# Common

sudo apt-get install acpi stow tldr git-doc diffutils-doc perl-doc make
sudo apt-get install python-pip build-essential gdb manpages-dev binutils \
    binutils-doc cpp g++ gcc libasan5 liblsan0 libtsan0 libubsan1 libc6-dev \
    cpp-doc gcc-doc autoconf automake libtool flex bison libasan5-dbg \
    liblsan0-dbg libtsan0-dbg libubsan1-dbg glibc-doc make-doc \
    autoconf-archive gnu-standards autoconf-doc gettext bison-doc flex-doc \
    libgcc1-dbg libgomp1-dbg libitm1-dbg libatomic1-dbg libmpx2-dbg \
    libquadmath0-dbg gdb-doc gettext-doc libtool-doc m4-doc python-doc cmake \
    cmake-doc diffstat unzip pinentry-doc zip software-properties-common \
    colordiff valgrind linux-tools-generic libjemalloc2 python3-scipy \
    python-numpy-doc python3-pytest python3-numpy-dbg python-scipy-doc \
    unattended-upgrades screen colordiff fzf hexyl ripgrep fd-find
# Not named fd by default because fdclone (which I don't use) was first
sudo ln -sf /usr/bin/fdfind /usr/local/bin/fd
# Home
sudo apt-get install g++-8 gcc-8-doc libstdc++6-8-dbg libstdc++-8-doc \
    libboost-dev libboost-doc
# Work
sudo apt-get install pkg-config libev-dev libssl-dev libssl-doc libldap2-dev \
    zlib1g-dev libreadline-dev readline-doc ncurses-doc pex python-pex-doc \
    python-secretstorage-doc python-setuptools-doc openjdk-8-jre openjdk-8-jdk \
    maven clang-tools-8 clang-8-doc llvm-8-doc bear libcurl4-openssl-dev \
    libcurl4-doc docker docker-doc python3-pip apparmor-profiles

sudo usermod -aG docker laurynas
wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | sudo apt-key add -
sudo add-apt-repository \
     "deb http://apt.llvm.org/focal/ llvm-toolchain-focal-10 main"
sudo apt-get install clangd-10 clang-format-10
sudo update-alternatives --install /usr/bin/clangd clangd /usr/bin/clangd-10 100
sudo update-alternatives --install /usr/bin/clang-format clang-format \
    /usr/bin/clang-format-10 100
# MySQL development specific
sudo apt-get install ccache rapidjson-dev valgrind-dbg libboost-container-dev \
    libboost-doc clang clang-8-doc llvm-8-doc clang-format clang-tidy cppcheck \
    iwyu ncdu lcov ncurses-doc libaio-dev libssl-dev libreadline-dev \
    readline-doc liblz4-dev libre2-dev libicu-dev icu-doc zlib1g-dev \
    libevent-dev pkg-config libcurl4-gnutls-dev libcurl4-doc libpam0g-dev \
    libtirpc-dev libprotobuf-dev libldap2-dev libsasl2-dev libnuma-dev mecab \
    libprotoc-dev doxygen doxygen-doc graphviz graphviz-doc libedit-dev
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
# DOES NOT SURVIVE REBOOT
sudo sh -c "echo 1 > /sys/devices/system/cpu/intel_pstate/no_turbo"
# After each apt upgrade
# (https://twitter.com/trav_downs/status/1280004737455271936):
sudo ln -s /usr/lib/debug/lib/x86_64-linux-gnu/* /usr/lib/debug/usr/lib/x86_64-linux-gnu/

# Edit `/etc/sysctl.d/10-ptrace.conf` for `kernel.yama.ptrace_scope = 0`

sudo sh -c "echo 0 > /proc/sys/kernel/yama/ptrace_scope"

