import sequtils, strutils

type Term* = ref object of RootObj ## p.24 (first-order) term
  functor: string

proc `$`*(self: Term): string =
  result = "t:"&self.functor

type Variable* = ref object of Term

proc `$`*(self: Variable): string =
  "v:" & self.functor

type Constant* = ref object of Term

proc `$`*(self: Constant): string =
  "c:" & self.functor

type Structure* = ref object of Term
  terms*: seq[Term] ## subterms

proc `$`*(self: Structure): string =
  result = "s:" & self.functor & "("
  result &= self.terms.mapIt($it).join(",")
  result &= ")"
