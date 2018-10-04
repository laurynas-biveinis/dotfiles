#!/bin/bash

if [ -f ~/.noninteractive_init_private.bash ]; then
    source ~/.noninteractive_init_private.bash
fi

export PATH=~/usr/bin:/usr/local/bin:$PATH
