# dotfiles
My dotfiles and scripts. Specific for bash.

# prerequisites
brew (macOS), stow, git

# installation
`cd`

`git clone git@github.com:laurynas-biveinis/dotfiles.git`

`cd dotfiles`

`git remote add origin-ro https://github.com/laurynas-biveinis/dotfiles.git`

Create `~/.noninteractive_init_private.bash`, if needed.

Create `~/dotfiles/dotfiles/extra_modules` with extra modules to include, e.g. "emacs nightly"

Check that existing .bashrc .profile files are OK to overwrite, rm them, and
`cd ~/dotfiles && stow bash git scripts $(cat dotfiles/extra_modules)`

If "emacs" is enabled, then 
`brew install gnupg@1.4`

`ln -sf /usr/local/bin/gpg1 /usr/local/bin/gpg`

`ln -sf $HOME/Documents/secrets.el $HOME/secrets.el`

If "nightly" is enabled, then `launchctl load ~/Library/LaunchAgents/nightly.plist`

If "wakatime" is enabled then

`sudo pip install wakatime`

`mkdir -p ~/usr/src`

`cd ~/usr/src`

`git clone https://github.com/gjsheep/bash-wakatime.git`

# TODO new system setup
## Ubuntu
`apt-get install ccache acpi valgrind rapidjson-dev stow gdb gdb-doc valgrind-dbg python-pip binutils build-essential cpp g++ gcc libasan5 libc6-dev liblsan0 libtsan0 libubsan1 make manpages-dev binutils-doc cpp-doc gcc-8-doc libstdc++-8-dev libstdc++6-8-dbg libasan5-dbg liblsan0-dbg libtsan0-dbg libubsan1-dbg glibc-doc libstdc++-8-doc make-doc gcc-doc cmake libboost-container-dev cmake-doc libboost-doc clang clang-7-doc llvm-7-doc clang-format clang-tidy cppcheck iwyu`
## macOS
brew
`brew install stow git fzf z`

`sudo easy_install pip`
