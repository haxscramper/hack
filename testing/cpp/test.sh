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
clang++ -std=c++17 signals_slots_varargs.cpp

# Run target
./a.out
