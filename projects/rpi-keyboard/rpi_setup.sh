#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset

if [[ "$1" = "-h" ]]; then
    cat << EOF | groff -man -Tascii
.TH rpi_setup.sh 1 '2019-05-01' '0.2.0'
.SH NAME
rpi_setup.sh
.SH SYNOPSIS
rpi_setup.sh [block_device]
.SH DESCRIPTION
Install raspbian on the block device
EOF

    exit 1
fi

run() {
    colecho -w0 "$@"
    sudo "$@"
}

write() {
    colecho -w2 "Writing to $2:"
    echo -e "$1" | sed 's/^/    /'
    echo -e "$1" | sudo tee -a "$2" > /dev/null
}


I_DEVICE="/dev/sd$1" # Enter your installation device here
sdroot="/mnt/sdcard" # Mount point

colecho -i2 "Block devices:"
{ lsblk | head -n1 ; lsblk | grep "sd$1" ; } | sed 's/^/    /'

mkdir -p $sdroot

parts=$(lsblk -b |
        grep -e "sd${1}"[0-9] |
        sed -E 's/^.{5}//' |
        awk '{ print $4" "$1}' |
        sort -n |
        cut -d' ' -f2)

boot="${I_DEVICE}$(echo -e "$parts" | head -n1)"
root="${I_DEVICE}$(echo -e "$parts" | tail -n1)"

colecho "Boot partition: $boot"
colecho "Root partition: $root"

run mount "$root" "$sdroot"
run mount "$boot" "$sdroot/boot"

run touch "$sdroot/boot/ssh"
write "dtoverlay=dwc2" "$sdroot/boot/config.txt"
write "dwc2\nlibcomposite" "$sdroot/etc/modules"

colecho "Required modules were added to $sdroot/etc/modules"

IP_ADDR=7
IP_SUBNET=7

HOST_IP="192.168.$IP_SUBNET.1"
RPI_IP="192.168.$IP_SUBNET.$IP_ADDR"

colecho -i1 "RPI IP address: $RPI_IP"
colecho -i1 "Gateway IP address: $HOST_IP"

gadget_name="hax_usb"
gadget_file="$sdroot/usr/bin/$gadget_name"

run touch "$gadget_file"
run chmod +x "$gadget_file"


report_desc=$(cat "$PWD/report_desc")
#host_mac_address=$(ifconfig | grep -A8 enp | grep ether | awk '{print $2}')

gadget_creator=$(
cat << EOF
cd /sys/kernel/config/usb_gadget/
mkdir -p $gadget_name
cd $gadget_name
echo 0x1d6b > idVendor # Linux Foundation
echo 0x0104 > idProduct # Multifunction Composite Gadget
echo 0x0100 > bcdDevice # v1.0.0
echo 0x0200 > bcdUSB # USB2
mkdir -p strings/0x409
echo "fedcba9876543210" > strings/0x409/serialnumber
echo "Tobias Girstmair" > strings/0x409/manufacturer
echo "iSticktoit.net USB Device" > strings/0x409/product
mkdir -p configs/c.1/strings/0x409
echo "Config 1: ECM network" > configs/c.1/strings/0x409/configuration
echo 250 > configs/c.1/MaxPower
# Add functions here

mkdir -p functions/ecm.usb0
# first byte of address must be even
HOST="48:6f:73:74:50:43" # "HostPC"
SELF="42:61:64:55:53:42" # "BadUSB"
echo \$HOST > functions/ecm.usb0/host_addr
echo \$SELF > functions/ecm.usb0/dev_addr
ln -s functions/ecm.usb0 configs/c.1/

mkdir -p functions/hid.usb0
echo 1 > functions/hid.usb0/protocol
echo 1 > functions/hid.usb0/subclass
echo 8 > functions/hid.usb0/report_length
echo -ne $report_desc > functions/hid.usb0/report_desc
ln -s functions/hid.usb0 configs/c.1/

# End functions
ls /sys/class/udc > UDC

ifconfig usb0 $RPI_IP netmask 255.255.255.0 up
route add -net default gw $HOST_IP
EOF
)

creator_script="create_usb_gadgets"

colecho -w3 "Gadget creator script:"
colecho -i1 "It will run on RPI on each startup to create usb gadgets"
# echo -e "\n$gadget_creator\n" | sed 's/^/    /' | bat -pl bash


write "/usr/bin/gadget_creator" "$sdroot/etc/rc.local"

write "$gadget_creator"  "$sdroot/boot/gadget_creator" > /dev/null

colecho "Gadget creation script has been added to $sdroot/boot/gadget_creator"
colecho "it will run each time RPI starts up and create usb ehternet interface"
colecho "with ip address: $RPI_IP and gateway $HOST_IP"


cd /

#run umount "$boot"
#run umount "$root"
