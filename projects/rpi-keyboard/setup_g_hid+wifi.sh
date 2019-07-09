#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset
set -o errexit

print_error() {
    colecho -e2 "Missing some of the arguments."
    ./$0 -h
    exit 1
}

if [ "$#" -eq 0 ]; then
    print_error
fi

if [[ "$1" == "-h" ]]; then
    cat << EOF | groff -man -Tascii
.TH setup_g_hid+wifi.sh 1 '2019-05-05' '0.0.1'
.SH NAME
setup_g_hid+wifi.sh
.SH SYNOPSIS
setup_g_hid+wifi.sh /dev/sd[X]
.SH DESCRIPTION
description
EOF
    exit 1
fi

if [ "$#" -ne 1 ]; then
    print_error
fi


conf="wifi_config.json_private"
rpi_ip=$(jq .rpi_ip_addr $conf)
subnet=$(jq .subnet $conf)
router_ip=$(jq .router_ip $conf)
wifi_password=$(jq .wifi_password $conf)
wifi_name=$(jq .wifi_name $conf)

colecho -i1 "Using block device /dev/sd$1 for installation"
colecho -i1 "Mount path /mnt/sdcard"
colecho -i1 "Rpi ip address: 192.168.$subnet.$rpi_ip"
colecho -i1 "Router ip: 192.168.$subnet.$router_ip"
colecho -i1 "Wifi credentials: Name: $wifi_name Password: $wifi_password"

source common.sh

sdroot="/mnt/sdcard"
boot=$(get_partitions "$1" boot)
root=$(get_partitions "$1" root)

mkdir -p $sdroot

./mount.sh "$1" "$sdroot"
./backup_default_config.sh p "$sdroot"

run touch "$sdroot/boot/ssh"

wpa_config=$(cat << EOF
ctrl_interface=DIR=/var/run/wpa_supplicant GROUP=netdev
update_config=1
country=GB

network={
	ssid=$wifi_name
	psk=$wifi_password
	key_mgmt=WPA-PSK
}
EOF
)

colecho -i0 "Wpa config file:"

wpa_file="$sdroot/boot/wpa_supplicant.conf"

if [ -f "$wpa_file" ]; then
    run rm "$wpa_file"
fi

write "$wpa_config" "$sdroot/boot/wpa_supplicant.conf"


interfaces=$(
    cat << EOF
iface wlan0 inet static
    address 192.168.$subnet.$rpi_ip
    netmask 255.255.255.0
    gateway 192.168.$subnet.$router_ip
EOF
)

write "$interfaces" "$sdroot/etc/network/interfaces"
write "dwc2\ng_hid" "$sdroot/etc/modules"
write "dtoverlay=dwc2" "$sdroot/boot/config.txt"

show "$sdroot/etc/network/interfaces"

