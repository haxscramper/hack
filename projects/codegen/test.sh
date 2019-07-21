#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset
set -o errexit

echo main.rkt | entr -rc sh -c "./main.rkt && clang-format parse.cpp | bat -lc++"
