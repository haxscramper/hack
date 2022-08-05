#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

path=$(
    jq -r \
        '.dependencies[] | select(.name == "libgit2") | .include_paths[0]' \
        conanbuildinfo.json
)

function try_build() {
    clang++ \
        -fuse-ld=mold \
        -o git-user \
        -ferror-limit=4 \
        git-user.cpp \
        @conanbuildinfo.gcc || true

    echo "git user compile ok"
    ./git-user
    echo "git user run ok"
}

try_build
clang++ genwrapper.cpp \
    -std=c++2a \
    -ferror-limit=1 \
    -o genwrapper \
    -fuse-ld=mold \
    -g \
    -lclang-cpp \
    -lLLVM \
    @conanbuildinfo.gcc

./genwrapper \
    $path/git2/config.h \
    $path/git2/global.h \
    $path/git2/repository.h \
    -o=$PWD/gitwrap.hpp \
    -extra-arg=-I/usr/lib/clang/14.0.6/include

try_build

# cmake .
# make -j 12
