#!/bin/sh

set -eu

# Not actually meant for running as an invoked script, but rather as a sequence
# of steps to consult and copy and paste into the terminal, and to do some
# unscripted steps manually.

# Do after dotfiles setup

# For vterm
brew install cmake

brew tap d12frosted/emacs-plus
readonly EMACS_FORMULA=emacs-plus@31
brew install "$EMACS_FORMULA" --with-dbus --with-debug --with-imagemagick \
	--with-mailutils --with-xwidgets
# Apple Silicon
osascript -e \
	'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/'"$EMACS_FORMULA"'/Emacs.app" at posix file "/Applications" with properties {name:"Emacs.app"}'

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
