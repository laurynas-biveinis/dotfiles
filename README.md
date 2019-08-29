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

Install [Lithuanian Standard Keyboard Layout](http://ims.mii.lt/klav/tvarkyk.html)

```bash
sudo scutil --set ComputerName new-computer-name
sudo scutil --set LocalHostName new-computer-name
sudo scutil --set HostName new-computer-name
sudo fdesetup enable
# Reboot (required by fdsetup enable)
# Show language menu in the login screen
sudo defaults write /Library/Preferences/com.apple.loginwindow showInputMenu -bool true
sudo fdesetup remove -user admin
sudo dscl . create /Users/admin IsHidden 1
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setglobalstate on
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setloggingmode on
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setstealthmode on
defaults -currentHost write ~/Library/Preferences/com.apple.alf -bool true
defaults write ~/Library/Preferences/com.apple.alf stealthenabled -bool true
sudo pkill -HUP socketfilterfw
# TimeMachine: do not ask to use new hard drives for backup
defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true
# Install XCode
# defaults write -g AppleLocale -string en_LT
# defaults write "Apple Global Domain" "AppleInterfaceStyle" "Dark"
defaults write -g KeyRepeat -int 2
defaults write -g InitialKeyRepeat -int 35
sudo defaults write com.apple.Safari AutoOpenSafeDownloads 0
defaults -currentHost write ~/Library/Preferences/com.apple.Safari WarnAboutFraudulentWebsites -bool true
defaults -currentHost write ~/Library/Preferences/com.apple.Safari TreatSHA1CertificatesAsInsecure -bool true
defaults -currentHost write ~/Library/Preferences/com.apple.Safari ShowFullURLInSearchField -bool true
chflags nohidden ~/Library/
sudo chflags nohidden /Volumes
# Hot Corners: screen saver on the bottom left corner
defaults write com.apple.dock wvous-bl-corner -int 5
defaults write com.apple.dock wvous-bl-modifier -int 0
# Finder
# Default view as list
defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"
defaults write com.apple.finder AppleShowAllFiles -bool true
deafults write com.apple.finder ShowPathbar -bool true
defaults write NSGlobalDomain AppleShowAllExtensions -bool true
sudo defaults -currentHost write /Library/Preferences/SystemConfiguration/com.apple.finder AppleShowAllFiles -bool true
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true
killall Dock
killall Finder
sudo launchctl load -w /System/Library/LaunchDaemons/ssh.plist
# Activity Monitor
# Show all processes
defaults write com.apple.ActivityMonitor ShowCategory -int 0
#
# Software Update
#
# Check for updates automatically
defaults write com.apple.SoftwareUpdate AutomaticCheckEnabled -bool true
# Check daily
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1
# Download updates in background automatically
defaults write com.apple.SoftwareUpdate AutomaticDownload -int 1
```

brew

```bash
brew install stow git z ncdu gnupg@1.4 fzf hexyl tldr lynis curl
sudo easy_install pip
```
