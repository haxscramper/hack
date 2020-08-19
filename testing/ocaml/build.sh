#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit

clang++ -std=c++17 earley_parser.cpp -o earley_parser
./earley_parser
