#!/bin/bash

export LANG=en_US.UTF-8

if [ -f ~/.noninteractive_init_private.bash ]; then
    # shellcheck disable=SC1090
    source ~/.noninteractive_init_private.bash
fi

if [ -d /usr/lib/ccache ]; then
    export PATH=/usr/lib/ccache:$PATH
fi

export HOMEBREW_NO_INSTALL_CLEANUP=y

export PATH=~/usr/bin:/usr/local/bin:/usr/local/sbin:$PATH

for script in ~/.bash.d/noninteractive_init/*; do
    # shellcheck disable=SC1090
    source "$script"
done
