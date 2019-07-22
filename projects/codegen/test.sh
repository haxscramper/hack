#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset
set -o errexit

fd -e nim | entr -rc sh -c "colecho -gi3 'Running entr' && nim c --cc:tcc -o:main.nim.bin main.nim && ./main.nim.bin && clang-format -style=file parse.cpp | bat -lc++ -p"
