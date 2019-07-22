import strformat
import sequtils
import strutils

type
  TypeProps = enum
    pStatic
    pConst
    pRef
    pPtr

  Type = object
    name: string
    props: seq[TypeProps]

  Var = object
    name: string
    vtyp: Type

type
  AcnKind = enum
    acnEnum
    acnClass
    acnFunction
  Acn = ref object
    name: string
    case kind: AcnKind
      of acnEnum:
        vFields: seq[Var]
      of acnClass:
        parents: seq[(string, string)]
        body: seq[Acn]
      of acnFunction:
        restype: Type
        args: seq[Var]

type
  CNode = ref object
    code: string
    under: seq[CNode]

proc cnode_to_string(cnode: CNode): string =
  cnode.code & " " & join(map(cnode.under, cnode_to_string), "\n")

proc acn_to_cnode(acn: Acn): CNode

proc acn_class_to_cnode(acn: Acn): CNode =
  CNode(
    code: &"class {acn.name}",
    under: concat(
      if acn.parents.len == 0:
        @[CNode(code: "{")]
      else:
        @[CNode(
          code: " : " &
            join(
              map(
                acn.parents,
                proc(par: (string, string)): string =
                  par[0] & " " & par[1] & " "),
              " , ")),
          CNode(code: "{")],
      map(acn.body, acn_to_cnode),
      @[CNode(code: "};")]))

proc acn_enum_to_cnode(acn: Acn): CNode =
  CNode(code: &"enum class {acn.name}")

proc acn_function_to_cnode(acn: Acn): CNode =
  CNode(
    under: @[
      CNode(code: acn.restype.name),
      CNode(code: acn.name)])


proc acn_to_cnode(acn: Acn): CNode =
  CNode(
    code: "",
    under: @[
      case acn.kind:
        of acnClass: acn_class_to_cnode(acn)
        of acnEnum: acn_enum_to_cnode(acn)
        of acnFunction: acn_function_to_cnode(acn)
  ])



echo cnode_to_string(acn_to_cnode(Acn(
  kind: acnClass,
  name: "QSTodo",
  parents: @[("public", "qde::DataItem")],
  body: @[Acn(
    kind: acnFunction,
    name: "hello",
    restype: Type(name: "int"),
    args: @[Var(name: "arg",
                vtyp: Type(name: "int"))])])))
