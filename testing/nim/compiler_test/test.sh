#!/usr/bin/env bash

cat << EOF > head.nim
proc farg(fa: proc(a: int) {.nimcall.}) = fa(12)

proc matcher(arg: int, pr: proc(arg: int): string {.nimcall.}): string =
  pr(arg)
EOF

cat << EOF > test.nim
discard compiles(matcher(0, toPStr))
EOF



cat head.nim > final.nim
echo "" >> final.nim
for i in $(seq 0 5000); do

   cat test.nim >> final.nim
done
time nim c --verbosity:0 --hints:off final.nim
