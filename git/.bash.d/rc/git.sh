#!/bin/sh

# shellcheck disable=SC2139
alias cf_on="$CF_ON"
# shellcheck disable=SC2139
alias cf_off="$CF_OFF"

# Decide and symlink the right git config
# https://stackoverflow.com/a/48998537/80458
if (echo a version 2.37.0; git --version) | sort -Vk3 | tail -1 | grep -q git
then
    ln -sf ~/.gitconfig.2.37 ~/.gitconfig
elif (echo a version 2.35.0; git --version) | sort -Vk3 | tail -1 | grep -q git
then
    ln -sf ~/.gitconfig.2.35 ~/.gitconfig
else
    ln -sf ~/.gitconfig.1.0 ~/.gitconfig
fi
