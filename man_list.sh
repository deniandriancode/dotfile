#!/bin/bash
arg=$(man -k . | my_dmenu_prompt.sh Manual | cut -d " " -f 1)
if [[ -n $arg ]]
then
    xfce4-terminal -e "man $arg"
fi
