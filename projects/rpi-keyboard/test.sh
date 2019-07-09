#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset
set -o errexit

# This file file will run on each update
colecho -i1 "Running test.sh"
##== Only edit lines after this comment

# Build target
./build.sh

# Run target
./setup_g_hid+wifi.sh d
