#!/bin/sh

brew install cmake cmake-language-server ninja

virtualenv /opt/virtualenvs/cmakelang
source /opt/virtualenvs/cmakelang/bin/activate
pip install cmakelang
sudo ln -sf /opt/virtualenvs/cmakelang/bin/cmakelang /usr/local/bin/cmakelang
deactivate
