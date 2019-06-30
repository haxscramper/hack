#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset
set -o errexit

colecho -i1 'Running run-dev.sh'

cat << EOF | entr sh -c "clear ; colecho -i1 'Running entr ...' ; ./test.sh"
signals_slots.hpp
signals_slots_varargs.cpp
grid_converter.cpp
test.sh
EOF

