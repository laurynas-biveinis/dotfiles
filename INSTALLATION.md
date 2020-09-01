# Installation

```bash
cd
git clone git@github.com:laurynas-biveinis/dotfiles.git
cd dotfiles
git remote add origin-ro https://github.com/laurynas-biveinis/dotfiles.git
```

Create `~/.noninteractive_init_private.bash`, if needed. Then `chmod 600` it.

Create `~/dotfiles/dotfiles/extra_modules` with extra modules to include, e.g.
"emacs nightly".

If "nightly" is one of those modules, `mkdir -p ~/Library/LaunchAgents`

Check that existing .bashrc .profile .bash_profile files are OK to overwrite, rm
them, and
`cd ~/dotfiles && stow $(cat dotfiles/base_modules) $(cat dotfiles/extra_modules)`

On macOS:

``` bash
ln -s ~/.gnupg/gpg-agent.conf.macOS ~/.gnupg/gpg-agent.conf
```

On Linux:

``` bash
ln -s ~/.gnupg/gpg-agent.conf.linux ~/.gnupg/gpg-agent.conf
```

If "emacs" is enabled, then

```bash
ln -sf $HOME/Documents/secrets.el $HOME/secrets.el
ln -sf $HOME/Documents/.hunspell_en_US $HOME/.hunspell_en_US
```

If "nightly" is enabled, then `launchctl load ~/Library/LaunchAgents/nightly.plist`

If "wakatime" is enabled then

```bash
sudo pip install wakatime
mkdir -p ~/usr/src
cd ~/usr/src
git clone https://github.com/gjsheep/bash-wakatime.git
```

put .wakatime.cfg into $HOME, `chmod 600` it.
