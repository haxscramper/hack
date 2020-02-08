#!/usr/bin/env bash

# DESC: generate header library from nim code and compile c or c++
# application using this library

#################################  notes  #################################
# TODO add linking of static and dunamic libraries
# TODO generate c++ library and link it
# TODO genrate javascript library and run code from it


#############################  configuration  #############################

compiler=gcc
target=c
libext=$(echo $target | tr 'c' 'h')
cache=nimcache_${compiler}_${target}

########################  ignore temporary files  #########################

ign=".gitignore"

touch $ign

if ! grep -qF "nimcace*/**" $ign; then
    echo "nimcache*/**" >> $ign
fi

rm -rf $cache

##########################  configuration debug  ##########################

cat << EOF
compiler  : $compiler
cache in  : $cache
target is : $target
libext is : $libext
EOF

####################  generate library from nim code  #####################

nim $target \
    --cc:$compiler \
    --noMain \
    --noLinking \
    --header:lib.$libext \
    --nimcache:$cache \
    lib.nim

####################  compile library into executable  ####################

nimdir=$HOME/.choosenim/toolchains/nim-$(
    nim --version | grep Version | cut -d' ' -f4 | tr -d '\n')

$compiler \
    -w \
    -I$nimdir/lib \
    -I$cache \
    $cache/*.$target \
    main.$target \
    -o main

#######################  run generated executable  ########################

./main