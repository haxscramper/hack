import test

proc test() {.qmethod, qconst, qoverride.} =
  echo "dd"

proc test1() {.parametric([qtconst, qtoverride]).} =
  echo "111"
