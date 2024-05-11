#!/bin/sh

UNAME_OUT="$(uname -s)"

TARGET=~/.gnupg/gpg-agent.conf

if [ "$UNAME_OUT" = "Darwin" ]; then
	if [ "$(arch)" = "arm64" ]; then
		ln -sf ~/.gnupg/gpg-agent.conf.macOS.apple-silicon $TARGET
	else
		ln -sf ~/.gnupg/gpg-agent.conf.macOS.intel $TARGET
	fi
else
	ln -sf ~/.gnupg/gpg-agent.conf.linux $TARGET
fi

unset TARGET
unset UNAME_OUT
