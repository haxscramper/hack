# from https://forum.nim-lang.org/t/8492#54922
import std/[macros, strutils, tables]

macro `~`(op: typed, body: untyped):untyped =
  case op.kind:
    of nnkCurly:#Table or set
      if body[0].kind == nnkAsgn:
        result = newNimNode(nnkTableConstr)
      else:
        result = copyNimTree(op)

    of nnkCall:
        result = copyNimTree(op)
        result.del(result.len - 1)

    of nnkSym:
      result = newCall(ident(op.strval))

    else:
      result = copyNimTree(op)

  var dest = result

  if op.kind == nnkPrefix and
     op[0].strval == "@":
    result = nnkPrefix.newTree(
      ident"@", newNimNode(nnkBracket))

    dest = result[1]

  for stmt in body:
    if stmt.kind == nnkAsgn:
      let res = newNimNode(nnkExprColonExpr)
      stmt.copyChildrenTo(res)
      dest.add res

    else:
      dest.add stmt




type
  Foo = object
    a: int
    b: set[char]

proc `$`(f: Foo): string = "Foo" & system.`$`(f)

proc weightedAvg(weight:float,data:varargs[float]):float =
  for d in data:
    result += d
  result = result * weight / data.len.float

proc foo(x,y,z:int):int = x + y * z

let nested = toTable: ~{}:
    "thing" = ~():
      "foo"
      ~[]:
        ~Foo():
          a = int: ~weightedAvg(0.5):
            11.0
            13.5
            19.3
          b = {'x'}
      ~ @[]:
        3.0
        3.1
        3.2
    "wrong" = ~():
      "bar"
      ~[]:
        ~Foo():
          a = ~foo:
            3
            4
            2
          b = ~{}:
            'y'
            'z'
      @[]

echo nested
