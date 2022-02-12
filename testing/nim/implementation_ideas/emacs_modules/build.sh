#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit

function gdb_cmd {
    gdb                                           \
        -batch                                    \
        -ex "set print address off"               \
        -ex "set print frame-arguments presence"  \
        -ex "run"                                 \
        -ex "bt"                                  \
        --args $@
}

# gcc -ggdb3 -Wall -fPIC -c mymod.c
# gcc -shared -o mymod.so mymod.o

emacs -Q --batch --directory "$PWD" -l mymod_user.el

nim c -o=haxloader.so haxloader.nim

# clang-format -i cache/@memacs_api.nim.c
emacs -Q --batch --directory "$PWD" -l main.el
