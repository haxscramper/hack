type
  Var = object
    name: string ## name of the variable
    vtyp: string ## type

type
  AcsType = enum
    acsPublic
    acsPrivate
    acsProtected


type
  ClsSection = ref object
    acsType : AcsType
    body : seq[ref Acn]
    comm : string

  AcnKind = enum
    acnEnum
    acnClass
    acnFunction
    acnPredicate
    acnIfStmt
    acnElseIfStmt
    acnElseStmt
    acnCode
    acnSwitch
    acnField

  Acn = ref object
    name: string
    body: seq[Acn]
    case kind: AcnKind
      of acnEnum:
        ## Names of the fields are stored in `name` fields of the
        ## variable
        eFields: seq[Var]
      of acnClass:
        parents: seq[(string, string)]
        sections: seq[ClsSection]

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
      of acnField:
        val: Var


type
  CNode = ref object
    code: string
    under: seq[CNode]
