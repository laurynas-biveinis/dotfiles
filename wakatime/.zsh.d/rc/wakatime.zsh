#!/bin/zsh
if [ -z "${ZSH_WAKATIME_BIN:-}" ] && type -p wakatime-cli >/dev/null 2>&1; then
	ZSH_WAKATIME_BIN=wakatime-cli
fi
zinit load sobolevn/wakatime-zsh-plugin
