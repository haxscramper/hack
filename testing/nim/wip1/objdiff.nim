# {.push warning[UnusedImport] = off.}

import hmisc/[htrie, hpprint]
import sugar, strutils, sequtils, strformat

## Experimental

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
