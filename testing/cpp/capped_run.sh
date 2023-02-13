#!/usr/bin/env bash
# -*- coding: utf-8 -*-
set -o nounset
set -o errexit

lim=6400000

ulimit -v $lim
ulimit -m $lim

$@
