#!/bin/zsh

setopt AUTO_CD
setopt CORRECT_ALL

bindkey " " magic-space

ulimit -c unlimited

#
# History
#
setopt EXTENDED_HISTORY
setopt SHARE_HISTORY
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY
HISTSIZE=100000000000
SAVEHIST=100000000000

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    # shellcheck disable=SC2015
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

export LSCOLORS=Exfxcxdxbxegedabagacad
export CLICOLOR=1
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

setopt NULL_GLOB
for script in ~/.zsh.d/rc/*; do
    source "$script"
done
unsetopt NULL_GLOB

alias rmcores="rm -rf /cores/*"

function strip_disas_offsets() {
    local input="$1"
    local output="$2"
    cut -f 2- -d ':' "$input" | sed 's/^\s*//g' > "$output"
}

function source_if_exists()
{
    local file=$1
    [ -f "$file" ] && source "$file"
}

source_if_exists ~/.fzf.zsh
source_if_exists ~/.nix-profile/etc/profile.d/nix.sh
