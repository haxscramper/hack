#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit

rm -rf build
mkdir -p build
cd build
cmake ..
make
