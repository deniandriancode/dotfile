#!/bin/bash

xfce4-terminal -e "man $(man -k . | my_dmenu.sh | cut -d " " -f 1)"


