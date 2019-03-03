#!/usr/bin/env bash
./converter.lisp 2>&1 | sed '/WARNING/d' | sed -E '/[0-9]{2}:/d'
clang-format -i "lisp_layout.scad"
