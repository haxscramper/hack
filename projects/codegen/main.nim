import strformat
import sequtils
import strutils
import osproc

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
    acnCode
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


proc var_to_string(t: Var): string = t.vtyp & " " & t.name
proc make_acn_code(str: string): Acn = Acn(kind: acnCode, code: str)


proc acn_enum_to_cnode(acn: Acn): CNode =
  CNode(
    code: "enum class $# {" % acn.name,
    under: concat(map(
      acn.eFields,
      proc (eVar: Var): CNode =
        CNode(code: eVar.name & ", ")), @[CNode(code: "};\n")]))


proc make_string_to_enum(
  acn: Acn,
  arg: Var = Var(
    name: "arg",
    vtyp: "const std::string&")): Acn =
  Acn(
    kind: acnFunction,
    restype: acn.name,
    name: "$#_to_string" % acn.name,
    args: @[arg],
    body: concat(map(
      acn.eFields,
      proc(field: Var): Acn =
        Acn(
          kind: acnIfStmt,
          cond: make_acn_code(
            "$# == \"$#\" " % @[arg.name, field.name]),
          body: @[make_acn_code("return $#::$# ;" % [acn.name, field.name])]))))


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

proc print_acn_tree(acn: Acn, level: int = 0) =
  let prefix = repeat(' ', level * 2)
  case acn.kind:
    of acnClass:
      echo prefix, "class"
    of acnEnum:
      echo prefix, "enum"
      for field in acn.eFields:
        echo prefix, "  ", field.name
    of acnFunction:
      echo prefix, "function ",
       join(map(acn.args, proc(v: Var): string = v.vtyp), " X "),
       " |-> ", acn.restype
    of acnPredicate:
      echo prefix, "predicate"
    of acnIfStmt:
      echo prefix, "if ", acn.cond.code
    of acnElseIfStmt:
      echo prefix, "else if"
    of acnElseStmt:
      echo prefix, "else"
    of acnCode:
      echo prefix, acn.code

  for node in acn.body:
    print_acn_tree(node, level + 1)

proc make_enum(name: string, eFields: seq[string]): Acn =
  Acn(
    kind: acnEnum,
    name: name,
    eFields: map(eFields, proc(str: string): Var = Var(name: str)))

proc make_enum(tmp: (string, seq[string])): Acn =
  make_enum(tmp[0], tmp[1])



var file = open("parse.cpp", fmWrite)
let enum_specs: seq[(string, seq[string])] =
  @[("Status", @["NoStatus", "Undefined", "Completed"])]



let acn_enums = map(enum_specs, make_enum)


let resAcnTree = Acn(
  kind: acnClass,
  name: "QSTodo",
  parents: @[("public", "qde::DataItem")],
  body: concat(
    acn_enums,
    map(
      acn_enums,
      proc(enm: Acn): Acn = make_string_to_enum(enm))))

print_acn_tree(resAcnTree)

write(file, (cnode_to_string(acn_to_cnode(resAcnTree))))

close(file)
