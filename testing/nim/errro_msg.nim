import sequtils, strutils

func acceptStr(str: string): string = discard

@["eee"].mapIt(it & "ee").filterIt(
  it.len == 1).mapIt(it.parseInt()).acceptStr()
