import colechopkg/lib
import strutils

const testing = true

proc dlog*(args: varargs[string, `$`]): void =
  when testing:
    ceUserLog0 args.join(" ")
  else:
    discard

var doDebug = false

proc toggleDebug*() = doDebug = not doDebug
proc enableDebug*() = doDebug = true
proc disableDebug*() = doDebug = false

proc decho*(args: varargs[string, `$`]): void =
  if doDebug: echo args.join("")
  else: discard

