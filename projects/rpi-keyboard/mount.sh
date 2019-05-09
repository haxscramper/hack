#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset
set -o errexit

source common.sh

if [[ $# -lt 1 ]]; then
    colecho -e1 "Invalid number of arguments."
    colecho -i2 "Specify block device"
    exit 1
fi

parts=$(lsblk -b |
            grep -e "sd${1}"[0-9] |
            sed -E 's/^.{5}//' |
            awk '{ print $4" "$1}' |
            sort -n |
            cut -d' ' -f2)


boot="/dev/sd${1}$(echo -e "$parts" | head -n1)"
root="/dev/sd${1}$(echo -e "$parts" | tail -n1)"


sdroot="${2:-/mnt/sdcard}"


colecho "Boot partition: $boot"
colecho "Root partition: $root"

if mountpoint -q "$sdroot"; then
    run umount "$sdroot/boot"
    run umount "$sdroot"
fi

run mount "$root" "$sdroot"
run mount "$boot" "$sdroot/boot"

run chmod +r "$sdroot/etc/wpa_supplicant/wpa_supplicant.conf"

case "${3:-none}" in
    nobackup)
        colecho "Done"
        ;;
    *)
        if [ -f "$sdroot/var/log/syslog" ]; then
            run cp "$sdroot/var/log/syslog" \
                "$HOME/defaultdirs/transient/rpi_logs/$(date -Is)_syslog"
        fi


        if [ -f "$sdroot/usb_setup_err" ]; then
            run cp "$sdroot/usb_setup_err" \
                "$HOME/defaultdirs/transient/rpi_logs/$(date -Is)_usb_setup_err"

        fi

        if [ -f "$sdroot/usr/bin/create_usb_gadget" ]; then
            run cp "$sdroot/usr/bin/create_usb_gadget" \
                "$HOME/defaultdirs/transient/rpi_logs/$(date -Is)_create_usb_gadget"

        fi

        colecho "Copied log files"
        ;;
esac
