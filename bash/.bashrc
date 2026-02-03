# Fig pre block. Keep at the top of this file.
[[ -f "$HOME/.fig/shell/bashrc.pre.bash" ]] && . "$HOME/.fig/shell/bashrc.pre.bash"
# shellcheck disable=SC1090
source ~/.noninteractive_init.bash

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

function source_if_exists()
{
    local file=$1
    [ -f "$file" ] && source "$file"
}

# So that Ctrl-s works for forward i-search
if [[ -t 1 ]]; then
    stty -ixon
fi

ulimit -c unlimited

export HISTCONTROL=ignoreboth:erasedups
# Unlimited history
export HISTFILESIZE=
export HISTSIZE=
# Prepend history entries with timestamps
export HISTTIMEFORMAT="[%F %T] "
# Append history, do not overwrite
shopt -s histappend

# append and reload the history after each command
# Taken from https://metaredux.com/posts/2020/07/07/supercharge-your-bash-history.html
PROMPT_COMMAND="history -a; history -n"

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
    # We have color support; assume it's compliant with Ecma-48
    # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
    # a case would tend to support setf rather than setaf.)
    color_prompt=yes
else
    color_prompt=
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_promp

# If this is an xterm set the title to user@host:dir
case "$TERM" in
    xterm*|rxvt*)
        # Remove the directive once shellcheck is upgraded
        # shellcheck disable=SC1117
        PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
        ;;
    *)
        ;;
esac

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

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        # shellcheck disable=SC1091
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        # shellcheck disable=SC1091
        . /etc/bash_completion
    fi
    if [ -d /usr/local/etc/bash_completion.d/ ]; then
        for script in /usr/local/etc/bash_completion.d/*; do
            source "$script"
        done
    fi
fi

for script in ~/.bash.d/rc/*; do
    source "$script"
done

#
# fzf
#
if [[ -d /usr/local/opt/fzf/bin ]]; then
    export PATH="$PATH:/usr/local/opt/fzf/bin"
fi

if [[ $- == *i* ]]; then
    if [[ -f /usr/local/opt/fzf/shell/completion.bash ]]; then
        # shellcheck disable=SC1091
        source /usr/local/opt/fzf/shell/completion.bash
    elif [[ -f /usr/share/doc/fzf/examples/completion.bash ]]; then
        # shellcheck disable=SC1091
        source /usr/share/doc/fzf/examples/completion.bash
    fi
fi

if [[ -f /usr/local/opt/fzf/shell/key-bindings.bash ]]; then
    # shellcheck disable=SC1091
    source /usr/local/opt/fzf/shell/key-bindings.bash
elif [[ -f /usr/share/doc/fzf/examples/key-bindings.bash ]]; then
    # shellcheck disable=SC1091
    source /usr/share/doc/fzf/examples/key-bindings.bash
fi

source_if_exists /usr/local/etc/profile.d/z.sh

alias rmcores="rm -rf /cores/*"

function strip_disas_offsets() {
    local input="$1"
    local output="$2"
    cut -f 2- -d ':' "$input" | sed 's/^\s*//g' > "$output"
}

# shellcheck disable=SC1091
# added by travis gem
[ -f /Users/laurynas/.travis/travis.sh ] && source /Users/laurynas/.travis/travis.sh

# Emacs vterm directory tracking
vterm_prompt_end(){
    printf "\e]51;A%s@%s:%s\e\\" "$(whoami)" "$(hostname)" "$(pwd)"
}
PS1=$PS1'\[$(vterm_prompt_end)\]'

# Hashicorp Vault
if [ -f /usr/local/bin/vault ]; then
    complete -C /usr/local/bin/vault vault
fi

# Fig post block. Keep at the bottom of this file.
[[ -f "$HOME/.fig/shell/bashrc.post.bash" ]] && . "$HOME/.fig/shell/bashrc.post.bash"
