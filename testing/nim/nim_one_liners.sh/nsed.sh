#!/usr/bin/env bash

echo "$@" > /tmp/file.nim
nim c --cc:tcc --hints:off --verbosity:0 -r /tmp/file.nim
