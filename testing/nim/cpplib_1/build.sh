#!/usr/bin/env bash

compiler=gcc
target=c
libext=$(echo $target | tr 'c' 'h')
cache=nimcache_${compiler}_${target}

ign=".gitignore"

touch $ign

if ! grep -qF "nimcace*/**" $ign; then
    echo "nimcache*/**" >> $ign
fi

rm -rf $cache

cat << EOF
compiler  : $compiler
cache in  : $cache
target is : $target
libext is : $libext
EOF


nim $target \
    --cc:$compiler \
    --noMain \
    --noLinking \
    --header:lib.$libext \
    --nimcache:$cache \
    lib.nim

nimdir=$HOME/.choosenim/toolchains/nim-$(
    nim --version | grep Version | cut -d' ' -f4 | tr -d '\n')

$compiler \
    -w \
    -I$nimdir/lib \
    -I$cache \
    $cache/*.$target \
    main.$target \
    -o main

./main
