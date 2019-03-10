#!/usr/bin/env bash
entr sh -c "timeout 60s ./test.sh" << EOF
converter.lisp
converter_test.lisp
test.sh
lisp_layout.json
EOF
