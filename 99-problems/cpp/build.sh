#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

file=p08

clang++ $file.cpp -std=c++20 -lgtest -o $file.bin
./$file.bin
