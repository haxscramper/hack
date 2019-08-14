#!/usr/bin/env bash
# -*- coding: utf-8 -*- bash
cd "$(dirname "$0")"
set -o nounset

bin_file="ncurses.c.bin"
tab_titl="${bin_file}.tab"

kitty @ set-tab-title "$tab_titl"

inotifywait -e close_write,moved_to,create -m . |
    while read -r directory events filename; do
        if [ "$filename" = "$bin_file" ]; then
            kitty @ close-window --match title:"$bin_file"
            kitty @ new-window --title "$bin_file" $(pwd)/"$bin_file"
            kitty @ goto-layout --match title:"$tab_titl" Stack
        elif [ "$filename" = "ncurses.c" ]; then
            gcc -lncurses -o "$bin_file" ncurses.c
        fi
    done
