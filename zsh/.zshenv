#!/bin/zsh

export LANG=en_US.UTF-8

if [ -f ~/.zshenv_private ]; then
    source ~/.zshenv_private
fi

export ZINIT_HOME=~/.zsh.d/zinit

if [ -d /usr/lib/ccache ]; then
    export PATH=/usr/lib/ccache:$PATH
fi

export HOMEBREW_NO_INSTALL_CLEANUP=y

export PATH=~/usr/bin:/usr/local/bin:/usr/local/sbin:$PATH

fpath+=~/.zsh.d/functions

setopt null_glob
for script in ~/.zsh.d/env/*; do
    source "$script"
done
unsetopt null_glob
