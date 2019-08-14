#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset

title="ncurses-window"

kitty @ close-window --match title:"$title"
echo 'Recompiled' $(date -Is)
kitty @ new-window --title "$title" $(pwd)/ncurses.c.bin
