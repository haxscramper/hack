#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

path=$(
    jq -r \
        '.dependencies[] | select(.name == "libgit2") | .include_paths[0]' \
        conanbuildinfo.json
)

function py_plotter() {
    ./plotter.py
    echo "py plotter ok"
}

function try_build() {
    clang++ \
        -fuse-ld=mold \
        -std=c++2a \
        -o git_user.bin \
        -O3 \
        -ferror-limit=8 \
        -lmatplot \
        git_user/git_user.cpp \
        @conanbuildinfo.gcc

    echo "git user compile ok"
    ./git_user.bin
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
        -o=$PWD/git_user/gitwrap.hpp \
        -extra-arg=-I/usr/lib/clang/14.0.6/include

}

# try_build
# build_git_wrapper
# wrap_git
try_build
# py_plotter
# cmake .
# make -j 12
