#!/bin/bash

arg=$(ls /bin | my_dmenu.sh)
if [[ -n $arg ]]; then
    xarg bash -c "$arg"
fi
