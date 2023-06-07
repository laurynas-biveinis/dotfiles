# Installation

```zsh
cd
git clone --recurse-submodules git@github.com:laurynas-biveinis/dotfiles.git
cd dotfiles
git remote add origin-ro https://github.com/laurynas-biveinis/dotfiles.git
```

Create `~/.zshenv_private`, if needed for any environment variables whose names
or values should be private or just different between systems, i.e. `NAME`,
`ORGANIZATION`, `MAKE_J`. Then `chmod 600` it.

Create `~/dotfiles/dotfiles/extra_modules` with extra modules to include, e.g.
"emacs nightly".

If "nightly" is one of those modules, `mkdir -p ~/Library/LaunchAgents`

Create potentially-empty directories so that `stow` does not symlink them:

```zsh
mkdir .gnupg
chmod 700 .gnupg
mkdir -p usr usr/bin usr/src vilniusdb
```

Check that existing `~/.zshenv` and `~/.zshrc` files are OK to overwrite, rm
them, and
`cd ~/dotfiles && stow $(cat dotfiles/base_modules) $(cat dotfiles/extra_modules)`

If "emacs" is enabled in `dotfiles/extra_modules`, then

```zsh
ln -sf $HOME/Documents/secrets.el $HOME/secrets.el
ln -sf $HOME/Documents/.hunspell_en_US $HOME/.hunspell_en_US
```

In Emacs, do `(all-the-icons-install-fonts)`.

If "nightly" is enabled, then `launchctl load ~/Library/LaunchAgents/nightly.plist`

If "wakatime" is enabled then, on macOS

```zsh
brew install wakatime-cli
```

On Linux

```bash
pip3 install wakatime
```

If using `zsh`, done. If using `bash`, then

```bash
cd ~/usr/src
git clone https://github.com/gjsheep/bash-wakatime.git
```

put .wakatime.cfg into $HOME, `chmod 600` it.

Change shell:

```bash
chsh
```
