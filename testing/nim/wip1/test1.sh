#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
IFS=","
watched="'$*'"
IFS=" "
fail_state="0"
msg="colecho -b"

$msg "Starting test script"

function build_test_tmp_pl {
  uname="perl_critic_0"
  fsm-build build --uname:"$uname"

  if [[ "$?" != "0" ]]; then
    colecho -e "Build failed"
    fail_state="1"
    return 1
  else
    fail_state="0"
  fi
}

function run_test_tmp_pl {
  build_test_tmp_pl
  if [[ "$fail_state" != "0" ]]; then
    return $fail_state
  fi

  uname="perl_critic_0"
  $msg -i:3 "Running $uname"

  ./test.tmp.pl
  echo
}

run_test_tmp_pl

fsm-build watch "[$watched]" |
  while read -r file; do
    if [[ "$file" == "test.tmp.pl" ]]; then
      run_test_tmp_pl
    fi
  done


