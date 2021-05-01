import std/macros
import hnimast
import hmisc/types/colorstring
import hmisc/helpers


proc treeRepr1*(
    pnode: NimNode,
    colored: bool = true,
    indexed: bool = false,
    maxdepth: int = 120
  ): string =

  proc aux(n: NimNode, level: int, idx: seq[int]): string =
    let pref =
      if indexed:
        idx.join("", ("[", "]")) & "    "
      else:
        "  ".repeat(level)

    if isNil(n):
      return pref & toRed("<nil>", colored)

    if level > maxdepth:
      return pref & " ..."

    result &= pref & ($n.kind)[3 ..^ 1]

    case n.kind:
      of nnkStrKinds:
        result &= " \"" & toYellow(n.getStrVal(), colored) & "\""

      of nnkIntKinds:
        result &= " " & toBlue($n.intVal, colored)

      of nnkFloatKinds:
        result &= " " & toMagenta($n.floatVal, colored)

      of nnkIdent, nnkSym:
        result &= " " & toGreen(n.strVal(), colored)

      of nnkCommentStmt:
        let lines = split(n.strVal(), '\n')
        if lines.len > 1:
          result &= "\n"
          for idx, line in pairs(lines):
            if idx != 0:
              result &= "\n"

            result &= pref & toYellow(line)

        else:
          result &= toYellow(n.strVal())

      else:
        if n.len > 0:
          result &= "\n"

        for newIdx, subn in n:
          result &= aux(subn, level + 1, idx & newIdx)
          if newIdx < n.len - 1:
            result &= "\n"

  return aux(pnode, 0, @[])

macro dumpObjectImpl(obj: typed): untyped =
  echo treeRepr1(obj)
  echo treeRepr1(obj.getTypeImpl())
  echo treeRepr1(obj.getTypeInst().getImpl())

template Eq(arg: string) {.pragma.}

type
  EqUser = object
    fld {.Eq("123").}: int

echo EqUser().fld.getCustomPragmaVal(Eq)

EqUser().dumpObjectImpl()
