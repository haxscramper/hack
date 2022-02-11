import ./emacs_api

proc returnValue(val: int, other: int = 12): int {.emcallp: "bind".} =
  result = val + 12

proc returnValue1(val: int): int {.emcall.} =
  result = 123

proc pointMax(env: EmEnv): int {.embind: "point-max".}
proc pointMin(env: EmEnv): int {.embind: "point-min".}

emInit():
  echo "initalized emacs"

  env.defun("test", 0..0, "doc"):
    return env.toEmacs(123)

  echo "point-max: ", env.pointMax()

  env.funcall("point-min", (1, 2, 3))
  env.defun(returnValueEmcall)

  echo "from emacs: ", env.pointMin()
