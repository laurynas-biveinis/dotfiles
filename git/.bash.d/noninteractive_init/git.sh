#!/bin/sh

cf_on() {
    git config --local include.path ../.gitconfig
}

cf_off() {
    git config --local --unset include.path
}
