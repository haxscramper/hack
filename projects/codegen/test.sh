#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset
set -o errexit

fd -e nim | entr -rc sh -c "echo 'Running entr' && \
nim c --verbosity:0 --hints:off --cc:tcc -o:main.nim.bin main.nim && \
./main.nim.bin && \
clang-format parse.cpp | bat -lc++ -p"
