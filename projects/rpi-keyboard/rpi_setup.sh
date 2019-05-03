#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset

if [[ "$1" = "-h" ]]; then
    cat << EOF | groff -man -Tascii
.TH rpi_setup.sh 1 '2019-05-01' '0.4.2'
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
    colecho -w0 -- "$@"
    sudo "$@"
}

write() {
    colecho -w2 -- "Writing to $2:"
    echo -e "$1" | sed 's/^/    /'
    echo -e "$1" | sudo tee -a "$2" > /dev/null
}


if [[ $# -ne 2 ]]; then
    colecho -e1 "Invalid number of arguments. Expected 2 got $#."
    colecho -i2 "Specify block device and PI ip address"
    exit 1
fi


I_DEVICE="/dev/sd$1" # Enter your installation device here
IP_ADDR="$2"
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

if mountpoint -q "$sdroot"; then
    run umount "$sdroot/boot"
    run umount "$sdroot"
fi

run mount "$root" "$sdroot"
run mount "$boot" "$sdroot/boot"

./backup_default_config.sh p "$sdroot"

run touch "$sdroot/boot/ssh"
write "dtoverlay=dwc2" "$sdroot/boot/config.txt"
write "dwc2\nlibcomposite" "$sdroot/etc/modules"

colecho "Required modules were added to $sdroot/etc/modules"

IP_SUBNET=7

HOST_IP="192.168.$IP_SUBNET.1"
RPI_IP="192.168.$IP_SUBNET.$IP_ADDR"

colecho -i1 "RPI IP address: $RPI_IP"
colecho -i1 "Gateway IP address: $HOST_IP"

gadget_name="hax_usb"


report_desc=$(cat "$PWD/report_desc")
host_mac_address=$(ifconfig | grep -A8 enp | grep ether | awk '{print $2}' | head -n1)

gadget_creator=$(
cat << EOF

err_file=\$HOME/usb_setup_err

echo "Time: \$(date -Ins)" >> \$err_file

echo 'Running setup script' >> \$err_file


{
##= Create usb gadget
cd /sys/kernel/config/usb_gadget/
mkdir -p hax_usb
cd hax_usb


##= Provide gadget configuration info
echo 0x1d6b > idVendor 
echo 0x0104 > idProduct
echo 0x0100 > bcdDevice
echo 0x0200 > bcdUSB 

mkdir -p strings/0x409
echo "fedcba9876543210" > strings/0x409/serialnumber
echo "Tobias Girstmair" > strings/0x409/manufacturer
echo "iSticktoit.net USB Device" > strings/0x409/product

##= Create gadget functions
mkdir -p functions/ecm.usb0
mkdir -p functions/hid.usb0

##== Describe emc function
HOST="48:6f:73:74:50:43" 
SELF="42:61:64:55:53:42"
echo \$HOST > functions/ecm.usb0/host_addr
echo \$SELF > functions/ecm.usb0/dev_addr
echo 'Created ethernet functions'

##== Descrive hid function
echo 1 > functions/hid.usb0/protocol
echo 1 > functions/hid.usb0/subclass
echo 8 > functions/hid.usb0/report_length
echo -ne $report_desc > functions/hid.usb0/report_desc
echo 'Created USB HID function'

##= Create gadget configuration
mkdir -p configs/c.1/strings/0x409
echo "Config 1: ECM network" > configs/c.1/strings/0x409/configuration
echo 250 > configs/c.1/MaxPower
echo 'Creating configuration'

##== Add functions to gadget configuration
ln -s functions/ecm.usb0 configs/c.1/
ln -s functions/hid.usb0 configs/c.1/
echo 'Added functions'


ls /sys/class/udc > UDC

echo 'Done'

cat UDC
} >> \$err_file 2>&1
EOF
)

script_name="create_usb_gadget"
creator_file="$sdroot/usr/bin/$script_name"

run rm -f "$creator_file"
run touch "$creator_file"
run chmod +x "$creator_file"

colecho -w3 "Gadget creator script: $creator_file"
colecho -i1 "It will run on RPI on each startup to create usb gadgets"
run sed -i 's/exit 0//' "$sdroot/etc/rc.local"
write "echo 'rc.local has been run' >> \$HOME/usb_setup_err" "$sdroot/etc/rc.local"
write "/usr/bin/$script_name" "$sdroot/etc/rc.local"
write "exit 0" "$sdroot/etc/rc.local"
write "$gadget_creator" "$creator_file" > /dev/null


colecho "Gadget creation script has been added to $creator_file"
colecho "it will run each time RPI starts up and create usb ehternet interface"
colecho "with ip address: $RPI_IP and gateway $HOST_IP"

bat -l bash "$sdroot/etc/rc.local"
bat --paging=never -l bash "$creator_file"
bat -l bash "$sdroot/etc/modules"
bat --line-range="40:" --paging=never -l bash "$sdroot/boot/config.txt"

colecho -i3 "DONE!!!"

cd /

#run umount "$boot"
#run umount "$root"
