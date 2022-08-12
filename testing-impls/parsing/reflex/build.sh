#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

reflex −−header-file lexerspec.l
clang++ lex.yy.cpp -lreflex -o lex.bin
echo "299d9dc9fv9e8e7f8g" | ./lex.bin

echo "done"
