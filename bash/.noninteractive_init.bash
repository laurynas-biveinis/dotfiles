#!/bin/bash

if [ -f ~/.noninteractive_init_private.bash ]; then
    # shellcheck disable=SC1090
    source ~/.noninteractive_init_private.bash
fi

if [ -d /usr/lib/ccache ]; then
    export PATH=/usr/lib/ccache:$PATH
fi

export LINUXBREW_PATH=/home/linuxbrew/.linuxbrew

if [ -d $LINUXBREW_PATH ]; then
    eval "$($LINUXBREW_PATH/bin/brew shellenv)"
fi

export PATH=~/usr/bin:/usr/local/bin:$PATH

for script in ~/.bash.d/noninteractive_init/*; do
    # shellcheck disable=SC1090
    source "$script"
done
