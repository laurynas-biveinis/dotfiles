#!/bin/zsh

setopt AUTO_CD
setopt CORRECT_ALL

#
# Completion
#
zstyle ':completion:*' list-suffixeszstyle ':completion:*' expand prefix suffix
autoload -Uz compinit && compinit

bindkey " " magic-space
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search

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

#
# Prompt
#
export PROMPT='%(?.%F{green}√%f.%F{red}?%?%f) %n@%m %B%~%b %# '
export RPROMPT='%*'

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

### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zinit-zsh/z-a-rust \
    zinit-zsh/z-a-as-monitor \
    zinit-zsh/z-a-patch-dl \
    zinit-zsh/z-a-bin-gem-node

### End of Zinit's installer chunk
