import strformat
import sequtils
import strutils
import osproc

include acn_creator

proc cnode_to_string(cnode: CNode): string =
  let head = if cnode.code != nil:
               cnode.code & " "
             else:
               ""

  let rest = if cnode.under != nil:
               join(map(cnode.under, cnode_to_string), "\n")
             else:
               ""

  return head & rest

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


proc acn_enum_to_cnode(acn: Acn): CNode =
  CNode(
    code: "enum class $# {" % acn.name,
    under: concat(map(
      acn.eFields,
      proc (eVar: Var): CNode =
        CNode(code: eVar.name & ", ")), @[CNode(code: "};\n")]))




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


proc acn_switch_to_cnode(acn: Acn): CNode =

  proc make_one_case(
    cs: tuple[
      case_var: string,
      action: Acn]): CNode =


    CNode(
      code:
      "case $#: { $# } break;" % [
        cs.case_var,
        cnode_to_string(acn_to_cnode(cs.action))])

  CNode(
    code: "switch ($#) {" % acn.swVar.name,
    under: map(acn.swCases, make_one_case))

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
        of acnSwitch: acn_switch_to_cnode(acn)
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
       # IDEA create long functions that spans several line, one line
       # for each variable
       # func |
       #      | -> restype
       #      |
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
    of acnSwitch:
      echo prefix, "switch ", acn.swVar.name
      echo join(
        map(
          acn.swCases,
          proc(cs: (string, Acn)): string = prefix & "  " & cs[0]),
        "\n")
    else:
      echo repr(acn.kind)

  for node in acn.body:
    print_acn_tree(node, level + 1)


var file = open("parse.cpp", fmWrite)
let enum_specs: seq[(string, seq[string])] =
  @[("Status", @["NoStatus", "Completed"])]



let acn_enums = map(enum_specs, make_enum)


let resAcnTree = Acn(
  kind: acnClass,
  name: "QSTodo",
  parents: @[("public", "qde::DataItem")],
  body: concat(
    acn_enums,
    map(acn_enums,
      proc(enm: Acn): Acn = make_string_to_enum(enm)),
    map(acn_enums,
        proc(enm: Acn): Acn =
          make_enum_to_string(enm))))

print_acn_tree(resAcnTree)

write(file, (cnode_to_string(acn_to_cnode(resAcnTree))))

close(file)
