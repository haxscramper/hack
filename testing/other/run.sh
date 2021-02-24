#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit

fd -e png -x realpath -- |
    perl -pe 's/^(.*)$/$1\0icon\x1f$1\n/' |
    rofi -dmenu -no-config -show-icons -theme style_simple.rasi
