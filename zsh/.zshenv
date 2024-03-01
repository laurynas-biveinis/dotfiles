#!/bin/zsh

export DOTFILES_ROOT=~/dotfiles

UNAME_OUT="$(uname -s)"

if [ "$UNAME_OUT" = "Darwin" ]; then
    export LANG=en_US.UTF-8
else
    export LANG=en_US.utf8
    export LC_CTYPE=en_US.utf8
fi


if [ -f ~/.zshenv_private ]; then
    source ~/.zshenv_private
fi

if [ -f /opt/homebrew/opt/asdf/libexec/asdf.sh ]; then
    source /opt/homebrew/opt/asdf/libexec/asdf.sh
fi

export ZINIT_HOME=~/.zsh.d/zinit

source ~/.zsh.d/paths

export HOMEBREW_NO_INSTALL_CLEANUP=y

fpath+=~/.zsh.d/functions

setopt null_glob
for script in ~/.zsh.d/env/*; do
    source "$script"
done
unsetopt null_glob
