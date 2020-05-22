#!/usr/bin/env bash

# Because SBCL has very convinient and easy-to-use controls for
# debugging

./add-ult.lisp 2>&1 | sed '/Backtrace for:/,$d'
# ./add-ult.lisp
