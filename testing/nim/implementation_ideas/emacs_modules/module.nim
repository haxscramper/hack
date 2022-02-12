import ./emacs_api

proc returnValue(val: int, other: int = 12): int {.emcallp: "bind".} =
  result = val + other

proc returnValue1(val: int): int {.emcall.} =
  result = 123

proc pointMax(env: EmEnv): int {.embind: "point-max".}
proc pointMin(env: EmEnv): int {.embind: "point-min".}
proc makeMarker(env: EmEnv): EmMarker {.embind: "make-marker".}
proc markerPosition(env: EmEnv, marker: EmMarker): OrNil[int] {.embind: "marker-position".}

emInit():
  echo "initalized emacs"

  env.defun("test", 0..0, "doc"):
    return env.toEmacs(123)

  echo "point-max: ", env.pointMax()
  echo "from emacs: ", env.pointMin()
