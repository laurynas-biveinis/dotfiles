#!/bin/zsh

set -euo pipefail

# Python. Do before cmake and mysql-work modules

asdf plugin add python
asdf install python latest
asdf set python 3.12.2 --home # Take from the output above
python -m pip install --upgrade pip setuptools
python -m pip install virtualenv
asdf reshim python
sudo mkdir -p /opt/virtualenvs
sudo chown "$(whoami)" /opt/virtualenvs

virtualenv /opt/virtualenvs/asitop
source /opt/virtualenvs/asitop/bin/activate
pip install asitop
sudo ln -sf /opt/virtualenvs/asitop/bin/asitop /usr/local/bin/asitop
deactivate

# TODO(laurynas): works with 3.12.2 but not 3.14.0t?
virtualenv /opt/virtualenvs/cppclean
source /opt/virtualenvs/cppclean/bin/activate
pip install cppclean
sudo ln -sf /opt/virtualenvs/cppclean/bin/cppclean /usr/local/bin/cppclean
deactivate

# Try to install Python development tools outside any virtualenv. If this
# breaks, then install per-project.
# The prefix of this command simplifies to "gpip" if user dotfiles are set up
PIP_REQUIRE_VIRTUALENV="0" pip install "python-lsp-server[all]" pylsp-mypy \
	python-lsp-isort pyls-memestra jedi pylsp-rope python-lsp-ruff \
	python-lsp-black pylint pycodestyle pyflakes pandas openpyxl \
	pandas-stubs scipy
