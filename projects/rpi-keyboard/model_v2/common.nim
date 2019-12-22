import colechopkg/lib
import strutils

const testing = true

proc dlog*(args: varargs[string, `$`]): void =
  when testing:
    ceUserLog0 args.join(" ")
  else:
    discard
