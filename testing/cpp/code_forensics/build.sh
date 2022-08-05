#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

path=$(
    jq -r \
        '.dependencies[] | select(.name == "libgit2") | .include_paths[0]' \
        conanbuildinfo.json
)

ROOT=$(realpath ..)

clang++ genwrapper.cpp \
    -std=c++2a \
    -ferror-limit=1 \
    -o genwrapper \
    -fuse-ld=mold \
    -I$ROOT/common \
    -g \
    -lclang-cpp \
    -lLLVM \
    @conanbuildinfo.gcc

./genwrapper \
    $path/git2/config.h \
    -o=/tmp/gitwrap.hpp \
    -extra-arg=-I/usr/lib/clang/14.0.6/include

# cmake .
# make -j 12
