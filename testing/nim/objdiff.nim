# {.push warning[UnusedImport] = off.}

import hmisc/[htrie, hpprint]
import sugar, strutils, sequtils, strformat

## Experimental

import typeinfo

template objKind(o: untyped): AnyKind =
  when o is bool:
    akBool                  #  a ``bool``
  elif o is char:
    akChar                  #  a ``char``
  elif o is enum:
      akEnum                  #  an enum
  elif o is array:
    akArray                 #  an array
  elif o is object:
    akObject                #  an object
  elif o is tuple:
    akTuple                 #  a tuple
  elif o is set:
    akSet                   #  a set
  elif o is range:
    akRange                 #  a range
  elif o is ptr: # Nim pointer type
    akPtr                   #  a ptr
  elif o is ref:
    akRef                   #  a ref
  elif o is seq:
    akSequence              #  a sequence
  elif (o is proc):
    akProc                  #  a proc
  elif o is pointer: # Opaque pointer to data
    akPointer               #  a pointer
  elif o is string:
    akString                #  a string
  elif o is string:
    akCString               #  a cstring
  elif o is int:
    akInt                   #  an int
  elif o is int8:
    akInt8                  #  an int8
  elif o is int16:
    akInt16                 #  an int16
  elif o is int32:
    akInt32                 #  an int32
  elif o is int64:
    akInt64                 #  an int64
  elif o is float:
    akFloat                 #  a float
  elif o is float32:
    akFloat32               #  a float32
  elif o is float64:
    akFloat64               #  a float64
  elif o is uint:
    akUInt                  #  an unsigned int
  elif o is uint8:
    akUInt8                 #  an unsigned int8
  elif o is uint16:
    akUInt16                #  an unsigned in16
  elif o is uint32:
    akUInt32                #  an unsigned int32
  elif o is uint64:
    akUInt64                #  an unsigned int64
  else:
    akNone                   ## invalid any

import macros


import hmisc/hterms_nimast

proc parseEnumSet[Enum](node: NimNode): set[Enum] =
  case node.kind:
    of nnkIdent:
      try:
        return {parseEnum[Enum]($node)}
      except ValueError:
        raise newException(
          ValueError,
          getCurrentExceptionMsg() & " for expression " & posString(node)
        )
    of nnkInfix:
      assert node[0] == ident("..")
      return {parseEnum[Enum]($node[1]) .. parseEnum[Enum]($node[2])}
    of nnkCurly:
      for subnode in node.children:
        result.incl parseEnumSet[Enum](subnode)

    else:
      discard

import hmisc/helpers

proc makeLiteralBranch(setExpr, caseBody: NimNode): NimNode =
  # echo "########"
  # echo setExpr.toStrLit()
  # echo caseBody.toStrLit()
  # defer:
  #   echo result.toStrLit()
  # echo "^^^^^^^"

  let setLiteral =
    case setExpr.kind:
      of nnkIdent: nnkCurly.newTree(setExpr)
      else: setExpr

  result = nnkElifExpr.newTree(
    nnkInfix.newTree(ident "in", ident "objType", setLiteral),
    caseBody
  )


macro switchType(expr, body: untyped): untyped =
  # echo body.treeRepr()
  var branchSets {.global, compiletime.}: set[AnyKind] = {akFloat128}
  var kindSet: set[AnyKind]
  for val in disjointIter(AnyKind):
    kindSet.incl val

  proc registerSet(node: NimNode, anchor: NimNode): void =
    let parsed = parseEnumSet[AnyKind](node)
    let diff = (branchSets * parsed) - {akFloat128}
    if diff.len > 0:
      raiseAssert(
        "Wrong type match: expression " & posString(anchor) &
          " is not disjoint from previous branches. Overlaps: " &
          $(diff)
      )
    else:
      branchSets.incl parsed

  echo "---"
  echo body.toStrLit()
  echo body.treeRepr()
  echo "---"

  var hasElse {.global, compiletime.} = false
  var branches {.global, compiletime.}: seq[NimNode]
  let rewrite = makeNodeRewriteSystem:
    rule:
      patt: Call([[setExpr]], [[caseBody]])
      outp:
        echo setExpr.toStrLit()
        registerSet(setExpr, setExpr)
        branches.add makeLiteralBranch(setExpr, caseBody)

        nnkStmtList.newTree()


    rule:
      patt: Infix(Ident(".."), [[start]], [[final]], [[caseBody]])
      outp:
        let setLiteral = nnkCurly.newTree(
            nnkInfix.newTree(ident "..", start, final)
        )

        registerSet(setLiteral, start)
        branches.add makeLiteralBranch(setLiteral, caseBody)

        nnkStmtList.newTree()

    rule:
      patt: Call([[setExpr]], [[caseBody]], Else([[elseBody]]))
      outp:
        block:
          registerSet(setExpr, setExpr)
          branches.add makeLiteralBranch(setExpr, caseBody)

        block:
          hasElse = true
          let diff = kindSet - branchSets
          let setLiteral = nnkCurly.newTree(toSeq(diff).mapIt(ident $it))
          registerSet(setLiteral, elseBody)
          branches.add makeLiteralBranch(setLiteral, caseBody)

        nnkStmtList.newTree()

    rule:
      patt: Infix(
        Ident(".."), [[start]], [[final]], [[caseBody]], [[elseBody]])
      outp:
        block:
          let setLiteral = nnkCurly.newTree(
              nnkInfix.newTree(ident "..", start, final)
          )

          registerSet(setLiteral, start)
          branches.add makeLiteralBranch(setLiteral, caseBody)

        block:
          hasElse = true
          let diff = kindSet - branchSets
          let setLiteral = nnkCurly.newTree(toSeq(diff).mapIt(ident $it))
          registerSet(setLiteral, elseBody)
          branches.add makeLiteralBranch(setLiteral, caseBody)

        nnkStmtList.newTree()



  let term = body.toTerm()
  let reduced = reduce(
    term, rewrite, nimAstImpl,
    reduceConstraints = rcRewriteOnce
  )

  if reduced.ok:
    let diff = (kindSet - branchSets)
    if diff.len > 0:
      raiseAssert("Not all cases are covered in type match " &
        posString(expr) & ". Missing " & $diff)

    result = nnkWhenStmt.newTree(branches)

    let kindId = ident "objType"
    result =
      quote do:
        const `kindId`: AnyKind = objKind(`expr`)
        `result`

    echo result.toStrLit()




func ppConst(val: string, ctype: string = ""): ObjTree =
  ObjTree(constType: ctype, kind: okConstant, strLit: val)

proc prettyPrintConverter*(val: Any): ObjTree =
  case val.kind:
    of akNone:
      return ppConst("<invalid>")
    of akBool:
      return ppConst($val.getBool(), "bool")
    of akChar:
      return ppConst($val.getChar(), "char")
    of akEnum:
      return ppConst($val.getEnumField(), "<enum>")
    of akSet:
      return ObjTree(
        kind: okSequence,
        valItems: toSeq(val.elements()).mapIt(ppConst($it))
      )
    of akObject, akTuple:
      return ObjTree(
        sectioned: false,
        kind: okComposed,
        fldPairs: toSeq(val.fields()).mapPairs(
          (name: lhs, value: prettyPrintConverter(rhs))
        )
      )
    of akRange:
      echo "RANGS????"

    of akRef, akPtr:
      return prettyPrintConverter(val[])

    of akSequence, akArray:
      return ObjTree(
        kind: okSequence,
        valItems:
          block:
            collect(newSeq):
              for i in 0 ..< val.len():
                prettyPrintConverter(val)
      )

    of akProc:
      echo "zzz"

    of akPointer:
      return ppConst("<ptr addr>", "<pointer>")

    of akString:
      return ppConst(val.getString(), "string")

    of akCString:
      return ppConst($val.getCString(), "cstring")

    of akInt:
      return ppConst($val.getInt(), "int")

    of akInt8:
      return ppConst($val.getInt8(), "int8")

    of akInt16:
      return ppConst($val.getInt16(), "int16")

    of akInt32:
      return ppConst($val.getInt32(), "int32")

    of akInt64:
      return ppConst($val.getInt64(), "int64")

    of akFloat:
      return ppConst($val.getFloat(), "float")

    of akFloat32:
      return ppConst($val.getFloat32(), "float32")

    of akFloat64:
      return ppConst($val.getFloat64(), "float64")

    of akUInt:
      return ppConst(system.`$`(val.getUInt()), "uint")

    of akUInt8:
      return ppConst(system.`$`(val.getUInt8()), "uint8")

    of akUInt16:
      return ppConst(system.`$`(val.getUInt16()), "uint16")

    of akUInt32:
      return ppConst(system.`$`(val.getUInt32()), "uint32")

    of akUInt64:
      return ppConst(system.`$`(val.getUInt64()), "uint64")

    else:
      discard

proc getField[Obj](obj: var Obj, path: openarray[int]): Any =
  if path.len == 1:
    return toAny[Obj](obj)

  switchType(obj):
    akNone:
      echo "none"
    akString .. akUInt64:
      return toAny(obj)
    else:
      echo "else"

type
  Tree = object
    f1: seq[Tree]
    f2: seq[Tree]
    val: int

block:
  var val = 12
  pprint getField(val, [1])

block:
  var val = Tree(f1: @[Tree(val: 12), Tree(val: 22)])
  pprint getField(val, [1])
