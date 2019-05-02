#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset
set -o errexit

sd="$2"

declare -a files

files[0]="$sd/etc/modules:backup_etc_modules.tmp"
files[1]="$sd/boot/config.txt:boot_config.txt.tmp"

if [ "$1" == "c" ]
then
    colecho "Copying config"
    for i in $(seq 0 $((${#files[@]} - 1))); do
        from=$(echo "${files[$i]}" | cut -d: -f1)
        to=$(echo "${files[$i]}" | cut -d: -f2)
        copy.sh "$from" "$to"
    done
elif [ "$1" == "p" ]
then
    colecho "Pasting config"
fi
