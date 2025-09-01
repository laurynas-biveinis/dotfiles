#!/bin/zsh

brew install cmake cmake-docs cmake-language-server ninja

virtualenv /opt/virtualenvs/cmakelang
source /opt/virtualenvs/cmakelang/bin/activate
pip install cmakelang
sudo ln -sf /opt/virtualenvs/cmakelang/bin/cmakelang /usr/local/bin/cmakelang
deactivate
