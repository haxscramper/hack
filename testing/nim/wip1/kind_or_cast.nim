import macros, strutils, strformat, parseutils
import hmisc/algo/halgorithm

func parseEnumField*(fld: NimNode): string =
  case fld.kind:
    of nnkEnumFieldDef:
      fld[0].strVal
    of nnkSym:
      fld.strVal
    else:
      raiseAssert(&"#[ IMPLEMENT {fld.kind} ]#")

func parseEnumImpl*(en: NimNode): seq[string] =
  case en.kind:
    of nnkSym:
      let impl = en.getTypeImpl()
      case impl.kind:
        of nnkBracketExpr:
          return parseEnumImpl(impl.getTypeInst()[1].getImpl())
        of nnkEnumTy:
          result = parseEnumImpl(impl)
        else:
          raiseAssert(&"#[ IMPLEMENT {impl.kind} ]#")
    of nnkTypeDef:
      result = parseEnumImpl(en[2])
    of nnkEnumTy:
      for fld in en[1..^1]:
        result.add parseEnumField(fld)
    of nnkTypeSection:
      result = parseEnumImpl(en[0])
    else:
      raiseAssert(&"#[ IMPLEMENT {en.kind} ]#")


func pref*(name: string): string =
  discard name.parseUntil(result, {'A' .. 'Z', '0' .. '9'})

macro hasKindImpl*(head: typed, kind: untyped): untyped =
  echo head.treeRepr()
  if head[0][0].strVal() == kind.strVal():
    result = newLit("true")
  else:
    let
      impl = head.getTypeImpl().parseEnumImpl()
      pref = impl.commonPrefix().pref()
      names = impl.dropPrefix(pref)
      kind = ident(kind.toStrLit().strVal().addPrefix(pref))

    result = nnkInfix.newTree(ident "==", head, kind)

template hasKind*(head, kindExpr: untyped): untyped =
  hasKindImpl(head.kind, kindExpr)

type
  En = enum
    eEn1
    eEn2

  Case = object
    case kind: En
      of eEn1:
        discard
      of eEn2:
        discard

echo Case().hasKind(Case)
echo Case().hasKind(En1)
echo Case().hasKind(En2)
