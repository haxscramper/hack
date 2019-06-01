#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset


# g++ -fdiagnostics-format=json p06.cpp 2>&1 | jq


g++ -fdiagnostics-format=json p06.cpp 2>&1 | jq .[].message
