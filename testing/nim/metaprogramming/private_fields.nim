import sugar, strutils, sequtils, strformat

#===========================  implementation  ============================#

type
  T = object
    f1: seq[T]
    f2: string

  U* = object
    f1: int
    f2: float
    f3: seq[T]
