#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit

nim c --verbosity:0 --app:lib dynlib_implementation.nim
nim c --verbosity:0 dynlib_loader.nim
./dynlib_loader
