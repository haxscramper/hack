import strformat
import sequtils
import strutils

type
  Var = object
    name: string
    vtyp: string

type
  AcnKind = enum
    acnEnum
    acnClass
    acnFunction
    acnPredicate
    acnIfStmt
    acnElseIfStmt
    acnElseStmt
    acnCode
  Acn = ref object
    name: string
    body: seq[Acn]
    case kind: AcnKind
      of acnEnum:
        vFields: seq[Var]
      of acnClass:
        parents: seq[(string, string)]
      of acnFunction:
        restype: string
        args: seq[Var]
      of acnPredicate, acnCode:
        code: string
      of acnIfStmt, acnElseIfStmt, acnElseStmt:
        cond: Acn


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

proc var_to_string(t: Var): string = t.vtyp & " " & t.name

proc acn_function_to_cnode(acn: Acn): CNode =
  CNode(
    under: @[
      CNode(code: acn.restype),
      CNode(code: acn.name),
      CNode(code: "("),
      CNode(code: join(map(acn.args, var_to_string), " ")),
      CNode(code: ") {"),
      CNode(under: map(acn.body, acn_to_cnode)),
      CNode(code: "}")
  ])

proc acn_pred_to_cnode(acn: Acn): CNode =
  CNode(code: acn.code)


proc body_to_cnodes(body: seq[Acn], closing: string = "}"): seq[CNode] =
  concat(
    map(body, acn_to_cnode),
    @[CNode(code: "}")])

proc acn_if_stmt_to_cnode(acn: Acn): CNode =
  CNode(
    code: "if ( $# ) {" % cnode_to_string(acn_pred_to_cnode(acn.cond)),
    under: body_to_cnodes(acn.body))



proc acn_else_if_stmt_to_cnode(acn: Acn): CNode =
  CNode(
    code: "else of ( $# ) {" %
    cnode_to_string(
      acn_pred_to_cnode(acn.cond)),
    under: body_to_cnodes(acn.body))

proc acn_else_stmt_to_cnode(acn: Acn): CNode =
  CNode(code: "else {", under: body_to_cnodes(acn.body))

proc acn_code_to_cnode(acn: Acn): CNode =
  CNode(code: acn.code)

proc acn_to_cnode(acn: Acn): CNode =
  CNode(
    code: "",
    under: @[
      case acn.kind:
        of acnClass: acn_class_to_cnode(acn)
        of acnEnum: acn_enum_to_cnode(acn)
        of acnFunction: acn_function_to_cnode(acn)
        of acnPredicate: acn_function_to_cnode(acn)
        of acnIfStmt: acn_if_stmt_to_cnode(acn)
        of acnElseIfStmt: acn_else_if_stmt_to_cnode(acn)
        of acnElseStmt: acn_else_stmt_to_cnode(acn)
        of acnCode: acn_code_to_cnode(acn)
  ])



echo cnode_to_string(acn_to_cnode(Acn(
  kind: acnClass,
  name: "QSTodo",
  parents: @[("public", "qde::DataItem")],
  body: @[Acn(
    kind: acnFunction,
    name: "hello",
    restype: "int",
    body: @[Acn(
      kind: acnIfStmt,
      cond: Acn(kind: acnPredicate, code: "1 == 2"),
      body: @[Acn(kind: acnCode, code: "return 0;")]
    )],
    args: @[Var(name: "arg", vtyp: "int")])])))
