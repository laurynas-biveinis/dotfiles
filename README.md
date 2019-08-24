# dotfiles
My dotfiles and scripts. Specific for bash.

# installation
```bash
cd
git clone git@github.com:laurynas-biveinis/dotfiles.git
cd dotfiles
git remote add origin-ro https://github.com/laurynas-biveinis/dotfiles.git
````

Create `~/.noninteractive_init_private.bash`, if needed.

Create `~/dotfiles/dotfiles/extra_modules` with extra modules to include, e.g. "emacs nightly"

Check that existing .bashrc .profile files are OK to overwrite, rm them, and
`cd ~/dotfiles && stow bash git scripts $(cat dotfiles/extra_modules)`

On macOS:
`/usr/local/opt/fzf/install`

If "emacs" is enabled, then 
```bash
brew install gnupg@1.4
ln -sf /usr/local/bin/gpg1 /usr/local/bin/gpg
ln -sf $HOME/Documents/secrets.el $HOME/secrets.el
```

If "nightly" is enabled, then `launchctl load ~/Library/LaunchAgents/nightly.plist`

If "wakatime" is enabled then
```bash
sudo pip install wakatime
mkdir -p ~/usr/src
cd ~/usr/src
git clone https://github.com/gjsheep/bash-wakatime.git
```

put .wakatime.cfg into $HOME

# TODO new system setup
## Ubuntu
`apt-get install build-essential ccache acpi valgrind rapidjson-dev stow gdb gdb-doc valgrind-dbg python-pip binutils cpp g++ gcc libasan5 libc6-dev liblsan0 libtsan0 libubsan1 make manpages-dev binutils-doc cpp-doc gcc-doc libstdc++-9-dev libstdc++6-9-dbg libasan5-dbg liblsan0-dbg libtsan0-dbg libubsan1-dbg glibc-doc libstdc++-9-doc make-doc gcc-doc cmake libboost-container-dev cmake-doc libboost-doc clang clang-8-doc llvm-8-doc clang-format clang-tidy cppcheck iwyu ncdu gnupg1 lcov autoconf automake libtool flex bison ncurses-doc python-doc autoconf-archive gnu-standards autoconf-doc gettext bison-doc fzf hexyl tldr libaio-dev libssl-dev libreadline-dev readline-doc liblz4-dev libre2-dev libicu-dev icu-doc zlib1g-dev libevent-dev pkg-config libcurl4-gnutls-dev libcurl4-doc libpam0g-dev libtirpc-dev libprotobuf-dev libldap2-dev libsasl2-dev libnuma-dev mecab libprotoc-dev doxygen doxygen-doc graphviz graphviz-doc libedit-dev`

Edit `/etc/sysctl.d/10-ptrace.conf` for `kernel.yama.ptrace_scope = 0`

`sudo sh -c "echo 0 > /proc/sys/kernel/yama/ptrace_scope"`

## macOS

Do applicable bits of [macOS Security and Privacy Guide](https://github.com/drduh/macOS-Security-and-Privacy-Guide)

```bash
defaults write -g AppleLocale -string lt_LT
defaults write "Apple Global Domain" "AppleInterfaceStyle" "Dark"
defaults write -g KeyRepeat -int 2
defaults write -g InitialKeyRepeat -int 35
defaults write com.apple.Safari AutoOpenSafeDownloads 0
chflags nohidden ~/Library/
defaults write com.apple.finder AppleShowAllFiles YES
```

brew

```bash
brew install stow git z ncdu gnupg@1.4 fzf hexyl tldr
sudo easy_install pip
```
