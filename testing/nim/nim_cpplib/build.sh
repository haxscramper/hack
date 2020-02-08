#!/usr/bin/env bash

nim cpp \
    --cc:gcc \
    --noMain \
    --noLinking \
    --header:mylib.h \
    --nimcache:nimcache \
    mylib.nim


nimdir=$HOME/.choosenim/toolchains/nim-$(
    nim --version | grep Version | cut -d' ' -f4 | tr -d '\n')

g++ \
    -I$nimdir/lib \
    -Inimcache \
    -o main main.cpp \
    nimcache/*.cpp -w


