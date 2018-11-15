# dotfiles
My dotfiles and scripts. Specific for bash.

# prerequisites
brew, stow, git

# installation
`cd`

`git clone git@github.com:laurynas-biveinis/dotfiles.git`

`cd dotfiles`

`git remote add origin-ro https://github.com/laurynas-biveinis/dotfiles.git`

Create `~/.noninteractive_init_private.bash`, if needed.

Create `~/dotfiles/dotfiles/extra_modules` with extra modules to include, e.g. "emacs nightly"

`cd ~/dotfiles && stow bash git scripts $(cat dotfiles/extra_modules)`

If "nightly" is enabled, then `launchctl load ~/Library/LaunchAgents/nightly.plist`
