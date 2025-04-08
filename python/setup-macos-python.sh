#!/bin/zsh
#
# Python
#

asdf plugin add python
asdf install python latest
asdf global python 3.12.2 # Take from the output above
python -m pip install --upgrade pip setuptools
python -m pip install virtualenv
asdf reshim python
sudo mkdir /opt/virtualenvs
sudo chown $(whoami) /opt/virtualenvs

virtualenv /opt/virtualenvs/asitop
source /opt/virtualenvs/asitop/bin/activate
pip install asitop
sudo ln -sf /opt/virtualenvs/asitop/bin/asitop /usr/local/bin/asitop
deactivate

virtualenv /opt/virtualenvs/cppclean
source /opt/virtualenvs/cppclean/bin/activate
pip install cppclean
sudo ln -sf /opt/virtualenvs/cppclean/bin/cppclean /usr/local/bin/cppclean
deactivate

# Try to install Python development tools outside any virtualenv. If this
# breaks, then install per-project.
gpip install "python-lsp-server[all]" pylsp-mypy python-lsp-isort \
     pyls-memestra jedi pylsp-rope python-lsp-ruff python-lsp-black pylint \
     pycodestyle pyflakes pandas openpyxl pandas-stubs scipy
