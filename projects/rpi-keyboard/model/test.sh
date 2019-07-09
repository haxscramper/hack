#!/usr/bin/env bash
colecho -i0 "Running test script"
# sbcl --script converter.lisp 2>&1 | sed '/WARNING/d' | sed -E '/[0-9]{2}:/d'
sbcl --script converter_test.lisp 2>&1 | sed '/WARNING/d' | sed -E '/[0-9]{2}:/d'
# clang-format -i "lisp_layout.scad"
# groff -ms -e notes.roff -Tpdf > res.pdf

for file in *.svg; do
    inkscape -z -e "${file%.svg}.png" -w 480 -h 480 $file
    bat -p $file
    colecho "Converted $file"
done
# cat -n lisp_layout.scad
