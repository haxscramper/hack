#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset

sd="$2"

declare -a files

files[0]="$sd/etc/modules:backup_etc_modules.tmp"
files[1]="$sd/boot/config.txt:boot_config.txt.tmp"
files[2]="$sd/etc/rc.local:rc_local.tmp"

colecho -i3 "Backup default config"


if [ "$1" == "n" ]
then
    colecho "Making backup of backup files"
    for file in *.tmp; do
        colecho "$file" "$file.tmp2"
        cp "$file" "$file.tmp2"
    done
elif [ "$1" == "c" ]
then
    colecho "Copying config"
    for i in $(seq 0 $((${#files[@]} - 1))); do
        from=$(echo "${files[$i]}" | cut -d: -f1)
        to=$(echo "${files[$i]}" | cut -d: -f2)
        colecho -i1 "Backup $from -> $to"
        rm "$to"
        cp "$from" "$to"
    done
elif [ "$1" == "p" ]
then
    colecho "Pasting config"
    for i in $(seq 0 $((${#files[@]} - 1))); do
        from=$(echo "${files[$i]}" | cut -d: -f2)
        to=$(echo "${files[$i]}" | cut -d: -f1)
        colecho -w1 "Restore $from -> $to"
        sudo cp "$from" "$to"
    done
fi
