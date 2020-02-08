#!/usr/bin/env bash

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

clang++ \
    -I$nimdir/lib \
    -Inimcache \
    -o main main.cpp \
    nimcache/*.cpp -w

./main
