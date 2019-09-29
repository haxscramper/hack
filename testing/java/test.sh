#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit
msg="colecho -b"

# ./template-run.pl


file="prt_3.java"

cd ".$file.tmp.d"

files=$(find  -name "*.java" | sed 's!./!!' | tr '\n' ',')

tpage \
    --define "files=$files" \
    --define "head=$HOME/tmp/java/$file.tex" \
    ../report_template \
    |
    tee out.tex |
    bat -p -llatex

latexmk -latexoption="-shell-escape" \
        -pdflua --interaction=nonstopmode out.tex > \
        /dev/null

