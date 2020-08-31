<!--- -*- gfm -*- -->

# dotfiles

My dotfiles and scripts, and configuration. Specific for bash.

## Modularity

The files are managed by [GNU Stow](https://www.gnu.org/software/stow/), divided
by tool or functional area: git, emacs, etc—see the top level-directories.
Different systems use different subsets of configuration, specified by some
files, as described in the [installation](#installation) section.

## Emacs

The biggest part of configuration is for Emacs, currently version 27.1. The
focus is on seamless integration between all the different packages, avoiding
surprises, and adding polish here and there. Of course, that is very much a work
in progress.

The bulk of configuration is in
[setup.el](https://github.com/laurynas-biveinis/dotfiles/blob/master/emacs/.emacs.d/setup.el),
there is also
[early-init.el](https://github.com/laurynas-biveinis/dotfiles/blob/master/emacs/.emacs.d/early-init.el)
and
[init.el](https://github.com/laurynas-biveinis/dotfiles/blob/master/emacs/.emacs.d/early-init.el).
There is also some system-specific setup over at
[darwin.el](https://github.com/laurynas-biveinis/dotfiles/blob/master/emacs/.emacs.d/darwin.el).

### Configured packages

I always prefer a melpa-stable version, if available and not broken.
Unfortunately, that is not always possible.

#### The big ones

[org](https://orgmode.org) [magit](https://magit.vc)
[lsp-mode](https://github.com/emacs-lsp/lsp-mode)
[flycheck](https://www.flycheck.org)
[company](https://github.com/company-mode/company-mode)
[helm](https://emacs-helm.github.io/helm/)
[projectile](https://github.com/bbatsov/helm-projectile)
[vterm](https://github.com/akermu/emacs-libvterm)
[deadgrep](https://github.com/Wilfred/deadgrep)
[undo-tree](http://www.dr-qubit.org/undo-tree.html)

#### Major

[GCMH](https://gitlab.com/koral/gcmh)
[cmake-build.el](https://github.com/rpav/cmake-build.el)
[neuron-mode](https://github.com/felko/neuron-mode)
[wgrep](https://github.com/mhayashi1120/Emacs-wgrep)
[google-c-style](https://github.com/google/styleguide/blob/gh-pages/google-c-style.el)

#### Nice-to-have, niche, & specific major modes

[rich-minority](https://github.com/Malabarba/rich-minority)
[dispwatch](https://github.com/mnp/dispwatch)
[lsp-treemacs](https://github.com/emacs-lsp/lsp-treemacs)
[calfw](https://github.com/kiwanami/emacs-calfw)
[helm-make](https://github.com/abo-abo/helm-make)
[which-key](https://github.com/justbur/emacs-which-key)
[keyfreq](https://github.com/dacap/keyfreq)
[helm-dash](https://github.com/dash-docs-el/helm-dash)
[helm-org](https://github.com/emacs-helm/helm-org)
[helm-lsp](https://github.com/emacs-lsp/helm-lsp)
[helm-projectile](https://github.com/bbatsov/helm-projectile)
[iedit](https://github.com/victorhge/iedit)
[eldoc-cmake](https://github.com/ikirill/eldoc-cmake)
[aggressive-indent-mode](https://github.com/Malabarba/aggressive-indent-mode)
[yaml-mode](https://github.com/yoshiki/yaml-mode)
[markdown-mode](https://jblevins.org/projects/markdown-mode/)
[ssh-config-mode](https://github.com/jhgorrell/ssh-config-mode-el)
[bison-mode](https://github.com/Wilfred/bison-mode)
[cmake-mode](https://github.com/Kitware/CMake/blob/master/Auxiliary/cmake-mode.el)

#### Appearance

[solarized-theme](https://github.com/bbatsov/solarized-emacs)
[git-gutter-fringe](https://github.com/emacsorphanage/git-gutter-fringe)
[helm-icons](https://github.com/yyoncho/helm-icons)
[company-box](https://github.com/sebastiencs/company-box)
[modern-cpp-font-lock](https://github.com/ludwigpacifici/modern-cpp-font-lock) [highlight-indent-guides](https://github.com/DarthFennec/highlight-indent-guides)
[page-break-lines](https://github.com/purcell/page-break-lines)
[xterm-color](https://github.com/atomontage/xterm-color)
[all-the-icons-dired](https://github.com/jtbm37/all-the-icons-dired)
[cmake-font-lock](https://github.com/Lindydancer/cmake-font-lock)

#### Packages bundled with Emacs

tramp erc cc-mode

### Annoyances fixed

* [Replace some cc-mode formatting commands with lsp-mode ones](https://github.com/laurynas-biveinis/dotfiles/blob/master/emacs/.emacs.d/setup.el#L1242)
* [macOS: add the missing man page paths for woman](https://www.reddit.com/r/emacs/comments/ig7zzo/weekly_tipstricketc_thread/g34s8dl?utm_source=share&utm_medium=web2x&context=3)
* [27.1 do GC if no frame has focus](https://www.reddit.com/r/emacs/comments/ibwzcu/weekly_tipstricketc_thread/g1zlh2t?utm_source=share&utm_medium=web2x&context=3)
* [Re-enable Shellcheck if using lsp-mode with bash-language-server](https://www.reddit.com/r/emacs/comments/hqxm5v/weekly_tipstricketc_thread/fy4pvr8?utm_source=share&utm_medium=web2x&context=3)
* Workaround the [emacs-wgrep
  issue](https://github.com/mhayashi1120/Emacs-wgrep/issues/75) of edited
  helm-grep buffers over TRAMP not applying their changes.
* Workaround the [projectile
  issue](https://github.com/bbatsov/projectile/issues/347) of remote projects
  not being added to Projectile project list.
* Workaround [projectile not being integrated with
  project.el](https://github.com/bbatsov/projectile/issues/1282) at least for
  xref.
* Integrate cmake-build.el with Projectile
* Integrate deadgrep with Projectile
* Fix [data corruption in
  deadgrep](https://github.com/Wilfred/deadgrep/issues/60) writable buffers by
  search-replace.

### Custom commands and functionality

* [Set frame geometry after docking/undocking laptop automatically](https://www.reddit.com/r/emacs/comments/ev2q9q/weekly_tipstricketc_thread/fftpfj0?utm_source=share&utm_medium=web2x&context=3)
* `my-recompile-packages`: force recompiling all the installed  packages, after
  a Emacs version upgrade or a borked package upgrade.

# Installation

```bash
cd
git clone git@github.com:laurynas-biveinis/dotfiles.git
cd dotfiles
git remote add origin-ro https://github.com/laurynas-biveinis/dotfiles.git
```

Create `~/.noninteractive_init_private.bash`, if needed. Then `chmod 600` it.

Create `~/dotfiles/dotfiles/extra_modules` with extra modules to include, e.g.
"emacs nightly".

If "nightly" is one of those modules, `mkdir -p ~/Library/LaunchAgents`

Check that existing .bashrc .profile .bash_profile files are OK to overwrite, rm
them, and
`cd ~/dotfiles && stow $(cat dotfiles/base_modules) $(cat dotfiles/extra_modules)`

On macOS:

``` bash
ln -s ~/.gnupg/gpg-agent.conf.macOS ~/.gnupg/gpg-agent.conf
```

On Linux:

``` bash
ln -s ~/.gnupg/gpg-agent.conf.linux ~/.gnupg/gpg-agent.conf
```

If "emacs" is enabled, then

```bash
ln -sf $HOME/Documents/secrets.el $HOME/secrets.el
ln -sf $HOME/Documents/.hunspell_en_US $HOME/.hunspell_en_US
```

If "nightly" is enabled, then `launchctl load ~/Library/LaunchAgents/nightly.plist`

If "wakatime" is enabled then

```bash
sudo pip install wakatime
mkdir -p ~/usr/src
cd ~/usr/src
git clone https://github.com/gjsheep/bash-wakatime.git
```

put .wakatime.cfg into $HOME, `chmod 600` it.

# New system setup

## Ubuntu

Common

```bash
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
sudo add-apt-repository "deb http://apt.llvm.org/focal/ llvm-toolchain-focal-10 main"
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

```

Edit `/etc/sysctl.d/10-ptrace.conf` for `kernel.yama.ptrace_scope = 0`

`sudo sh -c "echo 0 > /proc/sys/kernel/yama/ptrace_scope"`

## CentOS 6

```bash
sudovi # enable wheel
adduser laurynas
passwd laurynas
usermod -aG wheel laurynas
yum install epel-release openssh-clients -y # scp now works
# As user:
sudo yum install \
    http://opensource.wandisco.com/centos/6/git/x86_64/wandisco-git-release-6-1.noarch.rpm \
    -y
sudo yum install git -y
sudo yum install stow stow-doc python-pip gdb man-pages man-pages-overrides \
    binutils cpp gcc-c++ autoconf automake libtool flex bison gettext cmake \
    diffstat unzip zip colordiff wget -y
# Work
sudo yum install libev-devel openssl-devel openssl-static lua-devel lua-static \
    zlib-devel readline-devel java-1.8.0-openjdk java-1.8.0-openjdk-devel clang \
    libcurl-devel scl-utils cmake3 devtoolset-2-binutils devtoolset-2-gcc-c++ \
    openldap-devel -y
```

## macOS

Do applicable bits of [macOS Security and Privacy Guide](https://github.com/drduh/macOS-Security-and-Privacy-Guide)

Command-R, reboot, Disk Utility, Erase root partition, format as a APFS (not
encrypted, that will be enabled later). In the recovery mode terminal:

``` bash
csrutil enable --without dtrace
```

Command-Option-P-R on the first boot

Defaults script based on links from
[here](https://pawelgrzybek.com/change-macos-user-preferences-via-command-line/),
especially [this one](https://github.com/mathiasbynens/dotfiles/blob/master/.macos).

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
defaults write KeyRepeat -int 2
defaults write InitialKeyRepeat -int 35
# In System Preferences -> Keyboard -> Shortcuts:
# - uncheck "Turn keyboard access on or off ^F1"
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
# Esc closes autocompletion
defaults write -g NSUseSpellCheckerForCompletions -bool false
#
# Mouse
#
sudo defaults write -g com.apple.mouse.scaling -float 2
defaults write com.apple.AppleMultitouchMouse MouseButtonMode -string "TwoButton"
defaults write com.apple.driver.AppleBluetoothMultitouch.mouse MouseButtonMode \
    -string "TwoButton"
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
sudo fdesetup remove -user admin # TODO(laurynas)
sudo dscl . create /Users/admin IsHidden 1 # TODO(laurynas)
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
#
# Safari
#
defaults write
~/Library/Containers/com.apple.Safari/Data/Library/Preferences/com.apple.Safari \
    AutoOpenSafeDownloads -bool false
defaults -currentHost write ~/Library/Preferences/com.apple.Safari \
    WarnAboutFraudulentWebsites -bool true
defaults -currentHost write ~/Library/Preferences/com.apple.Safari \
    TreatSHA1CertificatesAsInsecure -bool true
defaults write com.apple.Safari ShowFullURLInSmartSearchField -bool true
# Cmd-W should only close tab, never window
defaults write com.apple.Safari NSUserKeyEquivalents -dict-add 'Close Tab' '<string>@w</string></dict>'
defaults write com.apple.universalaccess com.apple.custommenu.apps -array-add \
    '<string>com.apple.Safari</string>'
defaults write com.apple.Safari IncludeDevelopMenu -bool true
defaults write com.apple.Safari ShowFavoritesBar -bool false
# Enable continuous spellchecking, stolen from https://github.com/mathiasbynens/dotfiles
defaults write com.apple.Safari WebContinuousSpellCheckingEnabled -bool true
# Disable auto-correct
defaults write com.apple.Safari WebAutomaticSpellingCorrectionEnabled -bool false
defaults write com.apple.Safari AutoFillPasswords -bool false
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
defaults write com.apple.finder ShowStatusBar -bool true
# Show POSIX path in the window title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true
sudo defaults write NSGlobalDomain AppleShowAllExtensions -bool true
sudo defaults -currentHost write \
    /Library/Preferences/SystemConfiguration/com.apple.finder \
    AppleShowAllFiles -bool true
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true
defaults write -g AppleShowAllExtensions -bool true
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
#
# Image Capture
#
# Scan To path
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
bash create-darwin-volume.sh
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
```

brew

```bash
brew install stow git z ncdu gnupg coreutils fzf hexyl tldr lynis curl \
    shellcheck wget hunspell llvm duti grep ghostscript pinentry-mac findutils \
    libtool npm qemu fd delta unrar
sudo mkdir /Library/Spelling
sudo wget https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.aff
sudo wget https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.dic
sudo wget https://cgit.freedesktop.org/libreoffice/dictionaries/plain/lt_LT/lt.aff
sudo wget https://cgit.freedesktop.org/libreoffice/dictionaries/plain/lt_LT/lt.dic
# The 51st State
sudo ln -sf en_US.aff en_LT.aff
sudo ln -sf en_US.dic en_LT.dic
sudo easy_install pip
sudo gem install mdl
brew cask install emacs eve intel-power-gadget java rescuetime slack vlc \
    disk-inventory-x google-chrome pdftotext dash
# Evaluate emacs/.emacs.d/install-dash-docsets.el in Emacs, then install any
# non-main Dash docsets through the app
xattr -dr com.apple.quarantine "/Applications/Disk Inventory X.app"
duti -s org.videolan.vlc .MP4 all
duti -s org.videolan.vlc .mp3 all
duti -s org.videolan.vlc .m4a all
npm i -g bash-language-server
pip3 install cmake-language-server
# Work
brew cask install tunnelblick
# Play
brew cask install banktivity beatunes lastfm steam xld
duti -s jp.tmkk.XLD .flac all
```

# Dotfiles, Emacs distros, etc. I have been stealing from

* [Doom Emacs](https://github.com/hlissner/doom-emacs)
* [EmacsWiki: Dot Emacs Challenge](https://www.emacswiki.org/emacs/DotEmacsChallenge)
* [https://emacs.nasy.moe/]
