#!/bin/zsh

# Fig pre block. Keep at the top of this file.
if [ -f "$HOME/.fig/shell/zshrc.pre.zsh" ]; then
    . "$HOME/.fig/shell/zshrc.pre.zsh"
fi

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Decide and symlink the right git config
# https://stackoverflow.com/a/48998537/80458
if (echo a version 2.35.0; git --version) | sort -Vk3 | tail -1 | grep -q git
then
    ln -sf ~/.gitconfig.2.35 ~/.gitconfig
else
    ln -sf ~/.gitconfig.1.0 ~/.gitconfig
fi

setopt AUTO_CD
setopt CORRECT_ALL

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
HISTFILE=${ZDOTDIR:-$HOME}/.zsh_history

#
# less
#
[ -x /usr/bin/lesspipe ] && eval `eval lesspipe`

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

if [[ -f "~/.fzf.zsh" ]]; then
    source "~/.fzf.zsh"
else
    source_if_exists /usr/share/doc/fzf/examples/completion.zsh
    source_if_exists /usr/share/doc/fzf/examples/key-bindings.zsh
fi

source_if_exists ~/.nix-profile/etc/profile.d/nix.sh

#
# vterm integration
#
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] ||
                              [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%2~\a" }

### Added by Zinit's installer
if [[ ! -f $HOME/.zsh.d/zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zsh.d/zinit" && command chmod g-rwX "$HOME/.zsh.d/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.zsh.d/zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zsh.d/zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
      zdharma-continuum/zinit-annex-rust \
      zdharma-continuum/zinit-annex-as-monitor \
      zdharma-continuum/zinit-annex-patch-dl \
      zdharma-continuum/zinit-annex-bin-gem-node

### End of Zinit's installer chunk

zinit load willghatch/zsh-saneopt
zinit load zsh-users/zsh-completions
zinit load zsh-users/zsh-autosuggestions
zinit load ael-code/zsh-colored-man-pages
zinit load djui/alias-tips
if [[ `uname -s` == "Darwin" ]]; then
    zinit snippet \
          https://github.com/Homebrew/homebrew-command-not-found/blob/master/handler.sh
fi

setopt NULL_GLOB
for script in ~/.zsh.d/rc/*; do
    source "$script"
done
unsetopt NULL_GLOB

#
# Powerlevel10k
#
zinit ice depth=1; zinit light romkatv/powerlevel10k

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

#
# Completion
#
zstyle ':completion:*' list-suffixeszstyle ':completion:*' expand prefix suffix
autoload -Uz compinit && compinit

zinit load zdharma/fast-syntax-highlighting

# Fig post block. Keep at the bottom of this file.
if [ -f "$HOME/.fig/shell/zshrc.post.zsh" ]; then
    . "$HOME/.fig/shell/zshrc.post.zsh"
fi
