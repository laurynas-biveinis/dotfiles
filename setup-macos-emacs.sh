#!/bin/zsh

brew tap railwaycat/emacsmacport
brew install emacs-mac --with-native-compilation --with-unlimited-select \
     --with-librsvg --with-mac-metal  --with-natural-title-bar
# Intel
osascript -e \
          'tell application "Finder" to make alias file to POSIX file "/usr/local/opt/emacs-mac/Emacs.app" at POSIX file "/Applications"'
# Apple Silicon
osascript -e \
          'tell application "Finder" to make alias file to POSIX file "/opt/homebrew/opt/emacs-mac/Emacs.app" at POSIX file "/Applications"'

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
