#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset
set -o errexit

fd -e rkt | entr -rc sh -c "colecho -gi3 'Running entr' && ./main.rkt && clang-format -style=file parse.cpp | bat -lc++ -p"
