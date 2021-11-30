type
  CBase {.bycopy.} = object of RootObj

proc impl(args: varargs[CBase]) = discard

impl(CBase())

