#!/usr/bin/env bash

languagetool \
    -l en-US \
    -c utf-8 \
    --json \
    --rulefile rules.xml \
    file.txt > results.txt 2> tags.txt

cat results.txt | jq ".matches"
bat tags.txt
echo "done"
