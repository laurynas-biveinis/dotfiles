#!/bin/zsh

# Do applicable bits of [macOS Security and Privacy
# Guide](https://github.com/drduh/macOS-Security-and-Privacy-Guide)

# Boot into recovery mode: Command-R on Intel, hold power button on Apple
# Silicon, Disk Utility, Erase root partition, format as a APFS (not
# encrypted, that will be enabled later). In the recovery mode terminal:
# csrutil enable --without dtrace

# On Intel: Command-Option-P-R on the first boot

# Add Terminal.app to System Preferences -> Privacy -> Full Disk Access
#
# Lithuanian Standard Keyboard Layout (http://ims.mii.lt/klav/tvarkyk.html)
#
curl http://www.ims.mii.lt/klav/MacOS-X.zip --output macOS.zip
# Verify the downloaded file (note that lack of https above!)
unzip macOS.zip
sudo cp -r "Lithuanian Standard Keyboard.bundle" /Library/Keyboard\ Layouts
# Global and login screen setting
sudo defaults delete /Library/Preferences/com.apple.HIToolbox AppleEnabledInputSources
sudo defaults write /Library/Preferences/com.apple.HIToolbox \
    AppleEnabledInputSources -array-add \
    '<dict><key>InputSourceKind</key><string>Keyboard Layout</string><key>KeyboardLayout ID</key><integer>-4377</integer><key>KeyboardLayout Name</key><string>Lithuanian Standard</string></dict>'
# Show language menu in the login screen
sudo defaults write /Library/Preferences/com.apple.loginwindow \
    showInputMenu -bool true
# Current user setting
defaults delete com.apple.HIToolbox AppleEnabledInputSources
defaults write com.apple.HIToolbox AppleEnabledInputSources -array-add \
    '<dict><key>InputSourceKind</key><string>Keyboard Layout</string><key>KeyboardLayout ID</key><integer>-4377</integer><key>KeyboardLayout Name</key><string>Lithuanian Standard</string></dict>'
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
# Go to System Preferences -> Keyboard -> Shortcuts -> Input Sources, uncheck
# "Select the previous input source" and "Select next source in Input menu"
# The commands below do:
# - "Keyboard" group: uncheck "Turn keyboard access on or off ^F1"
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 12 \
         "{enabled = 0; value = { parameters = (65535, 97, 8650752); type = 'standard'; }; }"
# - uncheck "Mission Control ^-up" and "Application windows ^-down"
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 32 \
         "{enabled = 0; value = { parameters = (65535, 126, 8650752); type = 'standard'; }; }"
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 33 \
         "{enabled = 0; value = { parameters = (65535, 125, 8650752); type = 'standard'; }; }"
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 34 \
         "{enabled = 0; value = { parameters = (65535, 126, 8781824); type = 'standard'; }; }"
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 35 \
         "{enabled = 0; value = { parameters = (65535, 125, 8781824); type = 'standard'; }; }"
# - uncheck "Mission Control": "Move left a space", "Move right a space", and
# next/previous keyboard input source
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 60 \
         "{enabled = 0; value = { parameters = (32, 49, 262144); type = 'standard'; }; }"
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 61 \
         "{enabled = 0; value = { parameters = (32, 49, 786432); type = 'standard'; }; }"
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 79 \
         "{enabled = 0; value = { parameters = (65535, 123, 8650752); type = 'standard'; }; }"
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 80 \
         "{enabled = 0; value = { parameters = (65535, 123, 8781824); type = 'standard'; }; }"
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 81 \
         "{enabled = 0; value = { parameters = (65535, 124, 8650752); type = 'standard'; }; }"
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 82 \
         "{enabled = 0; value = { parameters = (65535, 124, 8781824); type = 'standard'; }; }"

# Esc closes autocompletion
defaults write -g NSUseSpellCheckerForCompletions -bool false
#
# Mouse
#
# TODO(laurynas): test after reboot
sudo defaults write -g com.apple.mouse.scaling -float 3
defaults write com.apple.AppleMultitouchMouse MouseButtonMode -string "TwoButton"
defaults write com.apple.driver.AppleBluetoothMultitouch.mouse MouseButtonMode \
         -string "TwoButton"
# Drag windows at any point with Ctrl-Opt-Cmd click, from
# https://twitter.com/nibroc/status/963088893758259200
defaults write -g NSWindowShouldDragOnGesture YES
#
# Appearance
#
defaults write "Apple Global Domain" "AppleInterfaceStyle" "Dark"
defaults write -g CGFontRenderingFontSmoothingDisabled -bool YES
#
# Computer name
#
sudo scutil --set ComputerName new-computer-name
sudo scutil --set LocalHostName new-computer-name
sudo scutil --set HostName new-computer-name
#
# Volume icon
#
defaults write com.apple.controlcenter.plist Sound -int 18
defaults write com.apple.systemuiserver menuExtras -array \
         "/System/Library/CoreServices/Menu Extras/Volume.menu"
killall SystemUIServer
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
#
# Power Management
#
sudo pmset -c sleep 0
sudo pmset -a DestroyFVKeyOnStandby 1
#
# Time Machine (set up manually)
#
# do not ask to use new hard drives for backup
defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true
#
# XCode
#
xcode-select --install
# /cores is root:wheel by default, where the default user is admin not wheel
sudo chown root:admin /cores
sudo chmod 775 /cores
#
# Screensaver
#
defaults -currentHost write com.apple.screensaver showClock -bool true
#
# Locale
#
defaults write -g AppleLocale -string en_LT
defaults write -g AppleMeasurementUnits -string "Centimeters"
#
# Time
#
sudo systemsetup -settimezone "Europe/Vilnius"
sudo systemsetup -setnetworktimeserver "time.euro.apple.com"
sudo systemsetup -setusingnetworktime on
sudo defaults write com.apple.menuextra.clock DateFormat -string \
     "EEE d MMM HH:mm:ss"
#
# Safari
#
defaults write \
    ~/Library/Containers/com.apple.Safari/Data/Library/Preferences/com.apple.Safari \
    AutoOpenSafeDownloads -bool false
defaults -currentHost write ~/Library/Preferences/com.apple.Safari \
    WarnAboutFraudulentWebsites -bool true
defaults -currentHost write ~/Library/Preferences/com.apple.Safari \
    TreatSHA1CertificatesAsInsecure -bool true
defaults write com.apple.Safari ShowFullURLInSmartSearchField -bool true
# Cmd-W should only close tab, never window
defaults write com.apple.Safari NSUserKeyEquivalents -dict-add 'Close Tab' \
         '<string>@w</string></dict>'
defaults write com.apple.universalaccess com.apple.custommenu.apps -array-add \
    '<string>com.apple.Safari</string>'
defaults write com.apple.Safari IncludeInternalDebugMenu -bool true
defaults write com.apple.Safari IncludeDevelopMenu -bool true
defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey \
         -bool true
defaults write com.apple.Safari ShowFavoritesBar -bool false
defaults write com.apple.Safari \
         com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled \
         -bool true
defaults write -g WebKitDeveloperExtras -bool true
# Enable continuous spellchecking, stolen from https://github.com/mathiasbynens/dotfiles
defaults write com.apple.Safari WebContinuousSpellCheckingEnabled -bool true
# Disable auto-correct
defaults write com.apple.Safari WebAutomaticSpellingCorrectionEnabled -bool false
defaults write com.apple.Safari AutoFillPasswords -bool false
#
# Terminal
#
open \
    ~/dotfiles/dotfiles/3rd_party/osx-terminal.app-colors-solarized/Solarized\ Dark.terminal
defaults write com.apple.Terminal "Default Window Settings" \
         -string "Solarized Dark"
defaults write com.apple.Terminal "Startup Window Settings" \
         -string "Solarized Dark"
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
defaults write com.apple.finder ShowStatusBar -bool true
# Show POSIX path in the window title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true
sudo defaults -currentHost write \
     /Library/Preferences/SystemConfiguration/com.apple.finder \
     AppleShowAllFiles -bool true
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true
defaults write -g AppleShowAllExtensions -bool true
# Allow text selection in Quick Look
defaults write com.apple.finder QLEnableTextSelection -bool true
killall Finder
#
# SSH
#
sudo launchctl load -w /System/Library/LaunchDaemons/ssh.plist
#
# Screen Sharing
#
sudo defaults write /var/db/launchd.db/com.apple.launchd/overrides.plist \
     com.apple.screensharing -dict Disabled -bool false
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
# Check daily
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1
# Download updates in background automatically
defaults write com.apple.SoftwareUpdate AutomaticDownload -int 1
#
# TextEdit
#
# Plain text by default
defaults write com.apple.TextEdit RichText -int 0
# Open and save files as UTF-8 in TextEdit
defaults write com.apple.TextEdit PlainTextEncoding -int 4
defaults write com.apple.TextEdit PlainTextEncodingForWrite -int 4
#
# Photos
#
# Do not open automatically when something is plugged in
defaults -currentHost write com.apple.ImageCapture disableHotPlug -bool true
#
# Preview
#
# Do not show sidebar
defaults write com.apple.Preview PVSidebarViewModeForNewDocuments -int 0
#
# Screenshots
#
defaults write com.apple.screencapture location -string "$HOME/Downloads"
defaults write com.apple.screencapture type -string "png"
defaults write com.apple.screencapture disable-shadow -bool true
#
# Image Capture
#
# Scan To path
# shellcheck disable=SC2088
defaults write com.apple.Image_Capture IK_Scanner_downloadURL -string "~/Downloads"
defaults write com.apple.Image_Capture IK_Scanner_selectedPathType -int 2
# PDF format
defaults write com.apple.Image_Capture IK_FileFormatTag -int 6
defaults write com.apple.Image_Capture IK_FileFormatTagText -int 6
# 150dpi
defaults write com.apple.Image_Capture IK_ScanResolution -int 150
# Do not Use Custom Size
defaults write com.apple.Image_Capture IK_UseCustomScanSize -int 0
# Show Details
defaults write com.apple.Image_Capture IK_scannerDisplayMode -int 1
#
# Printer
#
defaults write -g PMPrintingExpandedStateForPrint -bool TRUE
# Automatically quit printer app once the print jobs complete
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true
#
# Miscellaneous
#
chflags nohidden ~/Library/
sudo chflags nohidden /Volumes
#
# Nix package manager / neuron
#
sudo diskutil apfs addVolume disk1 APFS 'Nix Store' -mountpoint /nix
wget https://raw.githubusercontent.com/LnL7/nix/darwin-10.15-install/scripts/create-darwin-volume.sh
zsh create-darwin-volume.sh
sh <(curl -L https://nixos.org/nix/install) --daemon
nix-env -iA cachix -f https://cachix.org/api/v1/install
echo "trusted-users = root laurynas" | sudo tee -a /etc/nix/nix.conf \
    && sudo pkill nix-daemon
cachix use srid
nix-env -if https://github.com/srid/neuron/archive/master.tar.gz
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
# - Found no way to do System Preferences -> Dock & Menu Bar -> Spotlight ->
# uncheck "Show in Menu Bar" from command line

#
# brew
#
brew install stow git ncdu gnupg coreutils fzf hexyl tldr lynis curl java \
     shellcheck wget hunspell llvm duti grep ghostscript pinentry-mac htop \
     findutils libtool npm fd delta jq colordiff iwyu cppcheck infer creduce \
     gnu-sed mas bat fig bison libfido2 actionlint circleci imagemagick \
     rapidjson doxygen graphviz cmake protobuf ripgrep lz4 boost cpplint \
     libeatmydata
#
# App Store
#
mas install 497799835 # XCode
sudo xcodebuild -license accept
mas install 409201541 # Pages
mas install 409203825 # Numbers
mas install 1376402589 # StopTheMadness
mas install 1365531024 # 1Blocker
# Open App Store, login there
#
# Spellchecking
#
sudo mkdir /Library/Spelling
cd /Library/Spelling
sudo wget https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.aff
sudo wget https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.dic
sudo wget https://cgit.freedesktop.org/libreoffice/dictionaries/plain/lt_LT/lt.aff
sudo wget https://cgit.freedesktop.org/libreoffice/dictionaries/plain/lt_LT/lt.dic
# The 51st State
sudo ln -sf en_US.aff en_LT.aff
sudo ln -sf en_US.dic en_LT.dic
#
# Python
#
python3 -m pip install --upgrade pip
pip3 install asitop scipy pandas cmake-language-server cmakelang cppclean
sudo gem install mdl
brew install --cask rescuetime slack vlc disk-inventory-x google-chrome dash \
     telegram keycombiner michaelvillar-timer skype utm
# Start RescueTime, login

# Intel
brew install --cask intel-power-gadget
# Apple Silicon
brew install --cask mx-power-gadget

brew tap epk/epk
brew install font-sf-mono-nerd-font
# Set font in Terminal manually
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-native-comp
# Intel
osascript -e \
          'tell application "Finder" to make alias file to POSIX file "/usr/local/opt/emacs-mac/Emacs.app" at POSIX file "/Applications"'
# Apple Silicon
osascript -e \
          'tell application "Finder" to make alias file to POSIX file "/opt/homebrew/opt/emacs-mac/Emacs.app" at POSIX file "/Applications"'
sudo ln -sfn /usr/local/opt/openjdk/libexec/openjdk.jdk \
     /Library/Java/JavaVirtualMachines/openjdk.jdk
# Evaluate emacs/.emacs.d/install-dash-docsets.el in Emacs, then install any
# non-main Dash docsets through the app
xattr -dr com.apple.quarantine "/Applications/Disk Inventory X.app"
duti -s org.videolan.vlc .MP4 all
duti -s org.videolan.vlc .mp3 all
duti -s org.videolan.vlc .m4a all
npm i -g bash-language-server
brew tap homebrew/command-not-found
# Play
brew install mono
brew install --cask banktivity beatunes lastfm steam xld loopback audacity \
     guitar-pro transcribe
duti -s jp.tmkk.XLD .flac all
