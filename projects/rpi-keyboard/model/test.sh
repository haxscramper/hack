#!/usr/bin/env bash
cecho -i "Running test script"
# sbcl --script converter.lisp 2>&1 | sed '/WARNING/d' | sed -E '/[0-9]{2}:/d'
sbcl --script converter_test.lisp 2>&1 | sed '/WARNING/d' | sed -E '/[0-9]{2}:/d'
clang-format -i "lisp_layout.scad"
# cat -n lisp_layout.scad
