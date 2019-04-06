#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset
set -o errexit


cd /mnt/sdcard/boot
echo "dtoverlay=dwc2" | sudo tee -a config.txt
sudo sed -ie "s/\$/ modules-load=dwc2,g_ether/" cmdline.txt
sudo touch ssh
grep -q "modules-load=dwc2,g_ether" < cmdline.txt && echo "cmdline correct" && grep -q "dtoverlay=dwc2" < config.txt && echo "config correct"
cd ..
sudo touch ssh
