#!/usr/bin/env bash

set -o errexit

rm -rf nimcache

nim cpp \
    --cc:clang \
    --noMain \
    --noLinking \
    --header:mylib.h \
    --nimcache:nimcache \
    mylib.nim


nimdir=$HOME/.choosenim/toolchains/nim-$(
    nim --version | grep Version | cut -d' ' -f4 | tr -d '\n')

clang-format -i nimcache/mylib.h

echo "compiling executable"

clang++ \
    -I$nimdir/lib \
    -I$PWD \
    -Inimcache \
    -o main main.cpp \
    nimcache/*.cpp -w

./main
