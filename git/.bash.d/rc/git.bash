#!/bin/bash

export CF_ON="git config --local include.path ../.gitconfig"
export CF_OFF="git config --local --unset include.path"

# shellcheck disable=SC2139
alias cf_on="$CF_ON"
# shellcheck disable=SC2139
alias cf_off="$CF_OFF"
