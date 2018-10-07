source ~/.noninteractive_init.bash

# So that Ctrl-s works for forward i-search
stty -ixon

ulimit -c unlimited

export LC_TYPE=C
export LANG=C
export LC_CTYPE=C

export LSCOLORS=Exfxcxdxbxegedabagacad
export CLICOLOR=1

source $HOME/usr/src/bash-wakatime/bash-wakatime.sh

for script in $HOME/.bash.d/rc/*; do
    source $script
done
