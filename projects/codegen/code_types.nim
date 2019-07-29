type
  TypeSpec = enum
    const_t
    ptr_t
    ref_t

  TypeKind = enum
    hash_t
    vec_t
    string_t
    int_t
    enum_t
    other_t
  Type = ref object
    spec: seq[TypeSpec]
    case kind: TypeKind
      of hash_t:
        hKey: Type
        hVal: Type
      of vec_t:
        vItem: Type
      of other_t:
        oName: string
      of enum_t:
        eName: string
      of string_t:
        sName: string
      of int_t:
        nil


type
  Var = object
    name: string ## name of the variable
    vtyp: Type ## type

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
    acnWhile

  Acn = ref object
    name: string
    body: seq[Acn]
    comm: string
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
      of acnIfStmt, acnElseIfStmt, acnElseStmt, acnWhile:
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
