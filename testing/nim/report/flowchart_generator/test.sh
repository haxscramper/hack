#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit
msg="colecho -b"

f="flowchart_generator.nim"
bin="$f.bin"


nim c                 \
    --cc:tcc          \
    --verbosity:0     \
    --hints:off       \
    --debugger:native \
    -o:"$f.bin"       \
    $f

str="if (a) { 123; }"
./$bin --verbose-parse:"$str"
./$bin --from-string:"$str" --output-file:"out.tmp.dot"
./$bin --from-string:"$str" --dump-tree --output-file:"out.tmp.synt"
bat -lyaml "out.tmp.synt"
bat "out.tmp.dot"
# ./$bin --debug-parse:"$str"
