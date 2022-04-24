#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

nim c --nimcache:cache_c file.nim
clang-format -i cache_c/*.c
sed -i '/if (NIM_UNLIKELY(\*nimErr_))/ d' cache_c/*.c
clang-format -i cache_c/*.c
echo "nim c ok"
nim cpp --nimcache:cache_cpp file.nim
clang-format -i cache_cpp/*.cpp
echo "nim cpp ok"

nim r --stacktrace:on --linetrace:on cut_patterns.nim
