#!/bin/bash

if [ -f ~/.noninteractive_init_private.bash ]; then
    # shellcheck disable=SC1090
    source ~/.noninteractive_init_private.bash
fi

export PATH=~/usr/bin:/usr/local/bin:$PATH
