#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset

sudo umount -l /mnt/sdcard/boot
sudo umount -l /mnt/sdcard
