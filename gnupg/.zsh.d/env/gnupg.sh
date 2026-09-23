#!/bin/sh

if [ -x /opt/homebrew/bin/pinentry-mac ]; then
	GPG_AGENT_CONFIG=~/.gnupg/gpg-agent.conf.macOS.apple-silicon
elif [ -x /usr/local/bin/pinentry-mac ]; then
	GPG_AGENT_CONFIG=~/.gnupg/gpg-agent.conf.macOS.intel
else
	GPG_AGENT_CONFIG=~/.gnupg/gpg-agent.conf.linux
fi

if [ ! "$GPG_AGENT_CONFIG" -ef ~/.gnupg/gpg-agent.conf ]; then
	ln -sf "$GPG_AGENT_CONFIG" ~/.gnupg/gpg-agent.conf
fi

unset GPG_AGENT_CONFIG
