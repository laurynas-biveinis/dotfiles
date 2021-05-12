#!/bin/sh

# Common

sudo apt-get install stow tldr git-doc diffutils-doc perl-doc make \
     build-essential gdb manpages-dev binutils binutils-doc cpp g++ gcc \
     libasan5 liblsan0 libtsan0 libubsan1 libc6-dev cpp-doc gcc-doc autoconf \
     automake libtool flex bison glibc-doc make-doc autoconf-archive \
     gnu-standards autoconf-doc gettext bison-doc flex-doc gdb-doc gettext-doc \
     libtool-doc m4-doc cmake cmake-doc diffstat unzip pinentry-doc zip \
     software-properties-common colordiff valgrind linux-tools-generic \
     libjemalloc2 python3-scipy python-numpy-doc python3-pytest \
     python3-numpy-dbg python-scipy-doc unattended-upgrades screen colordiff \
     fzf fd-find clang moreutils psmisc zsh zsh-doc
# DeepState
sudo apt-get install libc6-dev-i386
# These two conflict with each other on 20.04:
sudo apt-get install hexyl
sudo apt-get install ripgrep
# If above failed due to '/usr/.crates2.json', then
sudo apt-get -o Dpkg::Options::="--force-overwrite" install ripgrep
# Not found on AWS EC2
sudo apt-get install acpi python-pip libasan5-dbg liblsan0-dbg libtsan0-dbg \
     libubsan1-dbg libgcc1-dbg libgomp1-dbg libitm1-dbg libatomic1-dbg \
     libmpx2-dbg libquadmath0-dbg python-doc
# Not named fd by default because fdclone (which I don't use) was first
sudo ln -sf /usr/bin/fdfind /usr/local/bin/fd
# So that I can have "gsed" as a GNU Sed on any platform
sudo ln -sf /bin/sed /bin/gsed
# Home
sudo apt-get install g++-10 gcc-10-doc libstdc++6-10-dbg libstdc++-10-doc \
     libboost-dev libboost-doc
# GCC 11
sudo add-apt-repository -y 'ppa:ubuntu-toolchain-r/test'
sudo apt-get install g++-11 gcc-11-doc libstdc++-11-doc
sudo usermod -aG docker laurynas
wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | sudo apt-key add -
sudo add-apt-repository \
     "deb http://apt.llvm.org/focal/ llvm-toolchain-focal-11 main"
sudo apt-get install clang-11 clang-tidy-11 llvm-11-dev lld-11 clangd-11 \
     clang-format-11
sudo update-alternatives --install /usr/bin/clangd clangd /usr/bin/clangd-11 100
sudo update-alternatives --install /usr/bin/clang-format clang-format \
     /usr/bin/clang-format-11 100
# Work
sudo apt-get install pkg-config libev-dev libssl-dev libssl-doc libldap2-dev \
    zlib1g-dev libreadline-dev readline-doc ncurses-doc pex python-pex-doc \
    python-secretstorage-doc python-setuptools-doc openjdk-8-jre openjdk-8-jdk \
    maven clang-tools-11 clang-11-doc llvm-11-doc bear libcurl4-openssl-dev \
    libcurl4-doc docker docker-doc python3-pip apparmor-profiles libuv1-dev \
    liblua5.1-dev
# MySQL development specific
sudo apt-get install ccache rapidjson-dev valgrind-dbg libboost-container-dev \
    libboost-doc clang-8-doc llvm-8-doc clang-format clang-tidy cppcheck \
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
