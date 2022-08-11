#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

file=p07

clang++ $file.cpp -o $file.bin
./$file.bin
