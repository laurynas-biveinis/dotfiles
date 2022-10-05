#!/bin/sh

# Decide and symlink the right git config
# https://stackoverflow.com/a/48998537/80458
if (echo a version 2.38.0; git --version) | sort -Vk3 | tail -1 | grep -q git
then
    ln -sf ~/.gitconfig.2.38 ~/.gitconfig
elif (echo a version 2.35.0; git --version) | sort -Vk3 | tail -1 | grep -q git
then
    ln -sf ~/.gitconfig.2.35 ~/.gitconfig
else
    ln -sf ~/.gitconfig.1.0 ~/.gitconfig
fi
