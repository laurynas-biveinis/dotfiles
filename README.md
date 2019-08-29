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

Command-R, reboot, Disk Utility, Erase root partition, format as a APFS (not encrypted, that will be enabled later)

Command-Option-P-R on the first boot

Defaults script based on links from [here](https://pawelgrzybek.com/change-macos-user-preferences-via-command-line/), especially [this one](https://github.com/mathiasbynens/dotfiles/blob/master/.macos).

```bash
#
# Lithuanian Standard Keyboard Layout (http://ims.mii.lt/klav/tvarkyk.html)
#
curl http://ims.mii.lt/klav/MacOS-X.zip --output macOS.zip
# Verify the downloaded file (note that lack of https above!)
unzip macOS.zip
sudo cp -r "Lithuanian Standard Keyboard.bundle" /Library/Keyboard\ Layouts
# Global and login screen setting
sudo defaults delete /Library/Preferences/com.apple.HIToolbox AppleEnabledInputSources
sudo defaults write /Library/Preferences/com.apple.HIToolbox AppleEnabledInputSources -array-add '<dict><key>InputSourceKind</key><string>Keyboard Layout</string><key>KeyboardLayout ID</key><integer>-4377</integer><key>KeyboardLayout Name</key><string>Lithuanian Standard</string></dict>'
# Show language menu in the login screen
sudo defaults write /Library/Preferences/com.apple.loginwindow showInputMenu -bool true
# Current user setting
defaults delete com.apple.HIToolbox AppleEnabledInputSources
defaults write com.apple.HIToolbox AppleEnabledInputSources -array-add '<dict><key>InputSourceKind</key><string>Keyboard Layout</string><key>KeyboardLayout ID</key><integer>-4377</integer><key>KeyboardLayout Name</key><string>Lithuanian Standard</string></dict>'
#
# Keyboard
#
sudo defaults write -g KeyRepeat -int 2
sudo defaults write -g InitialKeyRepeat -int 35
defaults write KeyRepeat -int 2
defaults write InitialKeyRepeat -int 35
#
# Computer name
#
sudo scutil --set ComputerName new-computer-name
sudo scutil --set LocalHostName new-computer-name
sudo scutil --set HostName new-computer-name
#
# Application Firewall
#
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setglobalstate on
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setloggingmode on
sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setstealthmode on
defaults -currentHost write ~/Library/Preferences/com.apple.alf -bool true
defaults write ~/Library/Preferences/com.apple.alf stealthenabled -bool true
sudo pkill -HUP socketfilterfw
#
# FileVault
#
sudo fdesetup enable
# Save the recovery key
# Reboot (required by fdsetup enable)
sudo fdesetup remove -user admin # TODO
sudo dscl . create /Users/admin IsHidden 1 # TODO
#
# TimeMachine (set up manually)
#
# do not ask to use new hard drives for backup
defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true
# Install XCode, start it once, accept EULA
#
# Safari
#
defaults write com.apple.Safari AutoOpenSafeDownloads 0
defaults -currentHost write ~/Library/Preferences/com.apple.Safari WarnAboutFraudulentWebsites -bool true
defaults -currentHost write ~/Library/Preferences/com.apple.Safari TreatSHA1CertificatesAsInsecure -bool true
defaults -currentHost write ~/Library/Preferences/com.apple.Safari ShowFullURLInSearchField -bool true
#
# Hot Corners: screen saver on the bottom left corner
#
defaults write com.apple.dock wvous-bl-corner -int 5
defaults write com.apple.dock wvous-bl-modifier -int 0
#
# Finder
#
# Default view as list
defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"
defaults write com.apple.finder AppleShowAllFiles -bool true
defaults write com.apple.finder ShowPathbar -bool true
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
#
# Photos
#
# Do not open automatically when something is plugged in
defaults -currentHost write com.apple.ImageCapture disableHotPlug -bool true
#
# Miscellaneous
#
chflags nohidden ~/Library/
sudo chflags nohidden /Volumes
#
# SetupAssistant defaults:
#
# defaults write -g AppleLocale -string en_LT
# defaults write "Apple Global Domain" "AppleInterfaceStyle" "Dark"
```

brew

```bash
brew install stow git z ncdu gnupg@1.4 fzf hexyl tldr lynis curl
sudo easy_install pip
```
