#!/bin/sh

export EDITOR="emacsclient -c -r"
export VISUAL="emacsclient -c -r"

if [ "$(uname -s)" = "Darwin" ]; then
	alias emacs-nw="emacs -nw"
else
	alias emacs-nw="TERM=xterm-24bit emacs -nw"
fi
