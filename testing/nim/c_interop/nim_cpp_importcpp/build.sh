#!/usr/bin/env bash

rm -rf nimcache

if ! nim cpp \
    --cc:clang \
    --noMain \
    --noLinking \
    --header:mylib.h \
    --nimcache:nimcache \
    mylib.nim
   then
       echo "nim build failed"
       exit 1
fi


nimdir=$HOME/.choosenim/toolchains/nim-$(
    nim --version | grep Version | cut -d' ' -f4 | tr -d '\n')

clang++ \
    -I$nimdir/lib \
    -Inimcache \
    -I$PWD \
    -o main main.cpp \
    nimcache/*.cpp -w

./main
