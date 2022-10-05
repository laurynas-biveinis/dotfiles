#!/bin/sh

UNAME_OUT="$(uname -s)"

if [ "$UNAME_OUT" = "Darwin" ]; then
    if [ "$(arch)" = "arm64" ]; then
        ln -sf ~/.gnupg/gpg-agent.conf.macOS.apple-silicon \
           ~/.gnupg/gpg-agent.conf
    else
        ln -sf ~/.gnupg/gpg-agent.conf.macOS.intel ~/.gnupg/gpg-agent.conf
    fi
else
    ln -sf ~/.gnupg/gpg-agent.conf.linux ~/.gnupg/gpg-agent.conf
fi
