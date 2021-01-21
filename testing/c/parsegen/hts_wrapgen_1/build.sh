#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit

rm -rf parser.c index.js

hts-wrapgen grammarFromFile \
            --grammar:grammar.js \
            --parserUser:user.nim \
            --parserOut:parser.c
