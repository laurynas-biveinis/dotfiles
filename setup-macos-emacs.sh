#!/bin/zsh

set -euo pipefail

# Do after dotfiles setup

brew tap d12frosted/emacs-plus
brew install emacs-plus --with-dbus --with-debug --with-imagemagick \
     --with-mailutils --with-xwidgets
# Apple Silicon
osascript -e \
          'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@30/Emacs.app" at posix file "/Applications" with properties {name:"Emacs.app"}'

npm install -g @emacs-eask/cli

# Evaluate emacs/.emacs.d/install-dash-docsets.el in Emacs, then install any
# non-main Dash docsets through the app
em-regen-info-dir
#
# For org-gcal:
touch ~/.emacs.d/oauth2-auto.plist
# For mu4e stack, copied from
# https://macowners.club/posts/email-emacs-mu4e-macos/
mkdir ~/.maildir
mkdir ~/.maildir/certificates
# 1. Open the Application Keychain Access.app
# 2. Select System Roots in the sidebar
# 3. Select all items listen here – ⌘ + a
# 4. Export the items with ⇧ + ⌘ + e to the file
#    ~/.maildir/certificates/root-certificates.pem
