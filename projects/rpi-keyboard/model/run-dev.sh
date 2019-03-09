#!/usr/bin/env bash
entr -c ./test.sh << EOF
converter.lisp
converter_test.lisp
test.sh
lisp_layout.json
EOF
