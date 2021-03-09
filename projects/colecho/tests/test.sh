#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd $(dirname "$0")

c=../colecho

$c --bg-col:red -I:3 -w:0 "Test"
$c --bg-col:blue -s:u

styles=( "gtest" )

for style in "${styles[@]}"
do
    echo "[$style]"
    $c -l:0 --$style "$style log 0"
    $c -i:0 --$style "$style info 0"
    $c -w:0 --$style "$style warn 0"
    $c -e:0 --$style "$style error 0"
done



$c -eg -- $(cat << EOF
newSeq[T] for creating new sequences of type T
@ for converting arrays and strings to sequences
add for adding new elements to strings and sequences
& for string and seq concatenation
in (alias for contains) and notin for checking if an item is in a container
EOF
)

$c -eg --rtr "Hello"
$c -egu "Hello"
$c -eLu -I:4 "Unifom log indent 4"
$c --help
