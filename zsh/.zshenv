#!/bin/zsh

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

export ZINIT_HOME=~/.zsh.d/zinit

if [ -d /usr/lib/ccache ]; then
    export PATH=/usr/lib/ccache:$PATH
fi

export HOMEBREW_NO_INSTALL_CLEANUP=y

export PATH=/usr/local/bin:/usr/local/bin:$PATH
if [ -d /opt/homebrew/bin ]; then
    export PATH=/opt/homebrew/bin:$PATH
fi
export PATH=~/usr/bin:$PATH

fpath+=~/.zsh.d/functions

setopt null_glob
for script in ~/.zsh.d/env/*; do
    source "$script"
done
unsetopt null_glob
