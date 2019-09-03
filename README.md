# dotfiles
My dotfiles and scripts. Specific for bash.

# installation
```bash
cd
git clone git@github.com:laurynas-biveinis/dotfiles.git
cd dotfiles
git remote add origin-ro https://github.com/laurynas-biveinis/dotfiles.git
````

Create `~/.noninteractive_init_private.bash`, if needed. Then `chmod 700` it.

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

put .wakatime.cfg into $HOME, `chmod 700` it.

# TODO new system setup
## Ubuntu
Common
```
apt-get install acpi stow gnupg1 fzf hexyl tldr
apt-get install python-pip build-essential gdb manpages-dev binutils binutils-doc cpp g++ gcc \
libasan5 liblsan0 libtsan0 libubsan1 libc6-dev make cpp-doc gcc-doc autoconf automake libtool \
flex bison libasan5-dbg liblsan0-dbg libtsan0-dbg libubsan1-dbg glibc-doc make-doc \
autoconf-archive gnu-standards autoconf-doc gettext bison-doc flex-doc libgcc1-dbg \
libgomp1-dbg libitm1-dbg libatomic1-dbg libmpx2-dbg libquadmath0-dbg gdb-doc gettext-doc \
libtool-doc m4-doc python-doc cmake cmake-doc
# Ubuntu 18.04-specific
apt-get install libstdc++6-8-dbg libstdc++-8-doc
# Ubuntu 19.04-specific
apt-get install libstdc++6-9-dbg libstdc++-9-doc
# Work
apt-get install pkg-config libev-dev libssl-dev libssl-doc libldap2-dev
# MySQL development specific
apt-get install ccache valgrind rapidjson-dev valgrind-dbg libboost-container-dev libboost-doc \
clang clang-8-doc llvm-8-doc clang-format clang-tidy cppcheck iwyu ncdu lcov ncurses-doc \
libaio-dev libssl-dev libreadline-dev readline-doc liblz4-dev libre2-dev libicu-dev icu-doc \
zlib1g-dev libevent-dev pkg-config libcurl4-gnutls-dev libcurl4-doc libpam0g-dev libtirpc-dev \
libprotobuf-dev libldap2-dev libsasl2-dev libnuma-dev \
mecab libprotoc-dev doxygen doxygen-doc graphviz graphviz-doc libedit-dev
```

Edit `/etc/sysctl.d/10-ptrace.conf` for `kernel.yama.ptrace_scope = 0`

`sudo sh -c "echo 0 > /proc/sys/kernel/yama/ptrace_scope"`

## macOS

Do applicable bits of [macOS Security and Privacy Guide](https://github.com/drduh/macOS-Security-and-Privacy-Guide)

Command-R, reboot, Disk Utility, Erase root partition, format as a APFS (not encrypted, that will be enabled later)

Command-Option-P-R on the first boot

Defaults script based on links from [here](https://pawelgrzybek.com/change-macos-user-preferences-via-command-line/), especially [this one](https://github.com/mathiasbynens/dotfiles/blob/master/.macos).

```bash
# Add Terminal.app to System Preferences -> Privacy -> Full Disk Access
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
# UI Controls
#
# Full keyboard control (e.g. Tab over buttons in modal dialogs)
defaults write -g AppleKeyboardUIMode -int 3
#
# Keyboard
#
sudo defaults write -g KeyRepeat -int 2
sudo defaults write -g InitialKeyRepeat -int 35
defaults write KeyRepeat -int 2
defaults write InitialKeyRepeat -int 35
#
# Mouse
#
sudo defaults write -g com.apple.mouse.scaling -float 2
defaults write com.apple.AppleMultitouchMouse MouseButtonMode -string "TwoButton"
defaults write com.apple.driver.AppleBluetoothMultitouch.mouse MouseButtonMode -string "TwoButton"
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
#
# Install XCode, start it once, accept EULA
#
#
#
# Screensaver
#
defaults -currentHost write com.apple.screensaver showClock -bool true
#
# Safari
#
defaults write ~/Library/Containers/com.apple.Safari/Data/Library/Preferences/com.apple.Safari AutoOpenSafeDownloads -bool false
defaults -currentHost write ~/Library/Preferences/com.apple.Safari WarnAboutFraudulentWebsites -bool true
defaults -currentHost write ~/Library/Preferences/com.apple.Safari TreatSHA1CertificatesAsInsecure -bool true
defaults write com.apple.Safari ShowFullURLInSmartSearchField -bool true
# Cmd-W should only close tab, never window
defaults write com.apple.Safari NSUserKeyEquivalents -dict-add 'Close Tab' '<string>@w</string></dict>'
defaults write com.apple.universalaccess com.apple.custommenu.apps -array-add '<string>com.apple.Safari</string>'
#
# Terminal
#
defaults write com.apple.Terminal "Default Window Settings" -string "Homebrew"
defaults write com.apple.Terminal "Startup Window Settings" -string "Homebrew"
#
# Dock
#
# Hot Corners: screen saver on the bottom right corner
defaults write com.apple.dock wvous-br-corner -int 5
defaults write com.apple.dock wvous-br-modifier -int 0
killall Dock
#
# Finder
#
# Default view as list
defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"
defaults write com.apple.finder AppleShowAllFiles -bool true
defaults write com.apple.finder ShowPathbar -bool true
# Show POSIX path in the window title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true
sudo defaults write NSGlobalDomain AppleShowAllExtensions -bool true
sudo defaults -currentHost write /Library/Preferences/SystemConfiguration/com.apple.finder AppleShowAllFiles -bool true
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true
killall Finder
#
# SSH
#
sudo launchctl load -w /System/Library/LaunchDaemons/ssh.plist
#
# Screen Sharing
#
sudo defaults write /var/db/launchd.db/com.apple.launchd/overrides.plist com.apple.screensharing -dict Disabled -bool false
sudo launchctl load -w /System/Library/LaunchDaemons/com.apple.screensharing.plist
#
# Activity Monitor
#
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
# Printer
#
# Automatically quit printer app once the print jobs complete
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true
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
#
# Things tried but not working due to various reasons, set up manually:
#
# - Desktop picture could be set, but isn't - see 
# https://www.tech-otaku.com/mac/setting-desktop-image-macos-mojave-from-command-line/ -
# scripting there is sensitive to e.g. number of monitors
# - NightShift recipes at e.g. 
# https://gist.github.com/thomasfinch/14bd3181799734c872d2ad3b207cc01c have no effect 
# on Mojave
# - Found no way to set "Show Time Machine in menu bar" from command line
```

brew

```bash
brew install stow git z ncdu gnupg@1.4 fzf hexyl tldr lynis curl shellcheck
sudo easy_install pip
brew cask install emacs eve intel-power-gadget rescuetime slack vlc virtualbox virtualbox-extension-pack
# Work: to be done
# Play
brew cask install banktivity beatunes disk-inventory-x google-chrome lastfm steam xld
```
