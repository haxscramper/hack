type
  Var = object
    name: string ## name of the variable
    vtyp: string ## type

type
  AcnKind = enum
    acnEnum
    acnClass
    acnFunction
    acnPredicate
    acnIfStmt
    acnElseIfStmt
    acnElseStmt
    acnCode,
    acnSwitch,
  Acn = ref object
    name: string
    body: seq[Acn]
    case kind: AcnKind
      of acnEnum:
        eFields: seq[Var]
      of acnClass:
        parents: seq[(string, string)]
      of acnFunction:
        restype: string
        args: seq[Var]
      of acnPredicate, acnCode:
        code: string
      of acnIfStmt, acnElseIfStmt, acnElseStmt:
        cond: Acn
      of acnSwitch:
        swVar: Var
        swCases: seq[(string, Acn)]
        swDefault: Acn


type
  CNode = ref object
    code: string
    under: seq[CNode]
