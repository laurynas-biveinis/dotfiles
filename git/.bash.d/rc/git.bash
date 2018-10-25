#!/bin/bash

if [ -f /usr/local/etc/bash_completion.d/git-completion.bash ]; then
    source /usr/local/etc/bash_completion.d/git-completion.bash
fi

alias cf_on="git config --local include.path ../.gitconfig"
alias cf_off="git config --local --unset include.path"
