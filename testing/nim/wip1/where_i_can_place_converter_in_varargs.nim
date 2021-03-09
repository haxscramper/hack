import sequtils, macros

proc default(a: varargs[string, `$`]) = echo a

default 1,2,3,4,5

macro varargsToSeq*(conv: untyped, args: varargs[untyped]): untyped =
  result = nnkBracket.newTree()
  for arg in args:
    result.add newCall(conv, arg)

  echo result.toStrLit()

template kvarargs*(args: varargs[untyped]) =
  echo varargsToSeq(toStrTuple, args).concat()

block:
  # Works as expected
  proc toStrTuple[A, B](args: openarray[(A, B)]): seq[(string, string)] =
    for arg in args:
      result.add ($arg[0], $arg[1])

  kvarargs [(22, 33)]
  kvarargs {"212" : "#33"}

block:
  # Works as expected
  proc toStrTuple[A, B](tpl: (A, B)): seq[(string, string)] =
    @[($tpl[0], $tpl[1])]

  kvarargs ("ee", "333")


block:
  # Works as expected
  proc toStrTuple[A, B](tpl: (A, B)): seq[(string, string)] =
    @[($tpl[0], $tpl[1])]

  proc toStrTuple[A, B](args: openarray[(A, B)]): seq[(string, string)] =
    for arg in args:
      result.add ($arg[0], $arg[1])

  kvarargs ("ee", "333")
  kvarargs [(22, 33)], ("h33", "h33"), [("eh", "hee")]
  kvarargs {"212" : "#33"}
