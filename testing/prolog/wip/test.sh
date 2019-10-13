#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit
msg="colecho -b"

swipl -f binary-matrix-square.splog -t halt
