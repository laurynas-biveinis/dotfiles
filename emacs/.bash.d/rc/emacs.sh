#!/bin/sh

export EDITOR="emacsclient -c -r"
export VISUAL="emacsclient -c -r"

UNAME_OUT="$(uname -s)"

if [ "$UNAME_OUT" = "Darwin" ]; then
    alias emacs-nw="emacs -nw"
else
    alias emacs-nw="TERM=xterm-24bit emacs -nw"
fi
