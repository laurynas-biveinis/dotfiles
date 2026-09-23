#!/bin/sh

(
	git_version=$(git --version) || exit
	git_version=${git_version#git version }
	git_major=${git_version%%.*}
	git_minor=${git_version#*.}
	git_minor=${git_minor%%.*}

	if [ "$git_major" -gt 2 ] || { [ "$git_major" -eq 2 ] && [ "$git_minor" -ge 38 ]; }; then
		git_config=~/.gitconfig.2.38
	elif [ "$git_major" -eq 2 ] && [ "$git_minor" -ge 35 ]; then
		git_config=~/.gitconfig.2.35
	else
		git_config=~/.gitconfig.1.0
	fi

	if [ ! "$git_config" -ef ~/.gitconfig ]; then
		ln -sf "$git_config" ~/.gitconfig
	fi
)

cf_on() {
	git config --local include.path ../.gitconfig
}

cf_off() {
	git config --local --unset include.path
}
