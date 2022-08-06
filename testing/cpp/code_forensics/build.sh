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
        -ferror-limit=8 \
        git-user.cpp \
        @conanbuildinfo.gcc || true

    echo "git user compile ok"
    ./git-user || true
    echo "git user run ok"
}

function build_git_wrapper() {
    clang++ genwrapper.cpp \
        -std=c++2a \
        -ferror-limit=1 \
        -o genwrapper \
        -fuse-ld=mold \
        -g \
        -lclang-cpp \
        -lLLVM \
        @conanbuildinfo.gcc

}

function wrap_git() {

    ./genwrapper \
        $path/git2.h \
        -o=$PWD/gitwrap.hpp \
        -extra-arg=-I/usr/lib/clang/14.0.6/include

}

try_build
# build_git_wrapper
# wrap_git
try_build

# cmake .
# make -j 12
