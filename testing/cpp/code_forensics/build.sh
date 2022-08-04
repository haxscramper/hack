#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

path=$(
    jq -r \
        '.dependencies[] | select(.name == "libgit2") | .include_paths[0]' \
        conanbuildinfo.json
)

clang++ genwrapper.cpp \
    -std=c++2a \
    -ferror-limit=1 \
    -o genwrapper \
    -g \
    -lclang-cpp \
    -lLLVM \
    @conanbuildinfo.gcc

./genwrapper \
    $path/git2 \
    -- \
    -extra-arg=-I/usr/lib/clang/14.0.6/include

# cmake .
# make -j 12
