import hmisc/helpers
import std/strformat

func funcall(arg: int): int = arg
proc procall(arg: int): int = arg
template tempcall(arg: int): int = arg

const count = 10_000_000

template recall(fun: untyped): untyped =
  for i in 0 .. count:
    let val = `fun`(i)
    discard val




proc otherProc() =
  timeIt "Function call":
    recall(funcall)

  timeIt "Proc call":
    recall(procall)

  timeIt "Temp call":
    recall(tempcall)

  timeIt "{.nimcall.}":
    let nimc = (proc(arg: int): int {.nimcall} = arg)

    recall(nimc)

  timeIt "{.closure.}":
    let closc = (proc(arg: int): int {.closure.} = arg)

    recall(closc)


  timeIt "{.closure.}, capture":
    let val = 10
    let closc = (proc(arg: int): int = val)

    recall(closc)

otherProc()
