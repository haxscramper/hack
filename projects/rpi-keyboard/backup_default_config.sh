#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset

sd="${2:-/mnt/sdcard}"

declare -a files

source common.sh

files[0]="$sd/etc/modules:backup_etc_modules.tmp"
files[1]="$sd/boot/config.txt:boot_config.txt.tmp"
files[2]="$sd/etc/rc.local:rc_local.tmp"
files[3]="$sd/etc/wpa_supplicant/wpa_supplicant.conf:wpa_supplicant.conf.tmp"
files[4]="$sd/etc/network/interfaces:etc_network_interfaces.tmp"

colecho -i3 "Backup default config"

if [ "$run_mode" == "test" ]; then
    colecho -i1 "Running in test mode"
fi


case "$1" in
    n)
        colecho "Making backup of backup files"
        for file in *.tmp; do
            colecho "$file" "$file.tmp2"
            cp "$file" "$file.tmp2"
        done
        ;;
    c)
        colecho "Copying config"
        for i in $(seq 0 $((${#files[@]} - 1))); do
            from=$(echo "${files[$i]}" | cut -d: -f1)
            to=$(echo "${files[$i]}" | cut -d: -f2)
            colecho -i1 "Backup $from -> $to"

            if [ -f "$to" ]; then
                rm "$to"
            fi

            if [ "$run_mode" != "test" ]; then
                cp "$from" "$to"
                colecho -I1 "Done"
            fi
        done
        ;;
    p)
        colecho "Pasting config"
        for i in $(seq 0 $((${#files[@]} - 1))); do
            from=$(echo "${files[$i]}" | cut -d: -f2)
            to=$(echo "${files[$i]}" | cut -d: -f1)
            colecho -w1 "Restore $from -> $to"
            if [ "$run_mode" != "test" ]; then
                sudo cp "$from" "$to"
            fi
        done
        ;;
    *)
        colecho -e2 "Invalild first argument. Use either (c)opy or (p)aste"
        ;;
esac
