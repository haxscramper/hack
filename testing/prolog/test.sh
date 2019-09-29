#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit
msg="colecho -b"

dr=.tmp.test

rm -f $dr/*
cp *.splog $dr/

cd $dr

