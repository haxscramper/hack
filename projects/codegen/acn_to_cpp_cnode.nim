include acn_creator


proc cnode_to_string(cnode: CNode): string =
  let head = if cnode.code != "":
               cnode.code & " "
             else:
               ""

  let rest = join(map(cnode.under, cnode_to_string), "\n")

  return head & rest


proc acn_to_cnode(acn: Acn): CNode


proc type_to_string(t: Type): string =
  var res = case t.kind:
    of int_t:
      "int"
    of enum_t:
      t.eName
    of other_t:
      t.oName
    of string_t:
      t.sName
    of vec_t:
      "std::vector< " & type_to_string(t.vItem) & ">"
    else:
      "[ TMP NOT IMPLEMENTED TYPE |-> STRING ]"


  res = if ptr_t in t.spec: res & "*" else: res
  res = if const_t in t.spec: "const " & res else: res

  return res

proc var_to_string(t: Var): string =
  type_to_string(t.vtyp) & " " & t.name



proc cls_section_to_cnode(sect: ClsSection): CNode =
  let sect_comm = if sect.comm != "":
                    "//#=== " & sect.comm & "\n"
                  else: ""

  let acs_type = case sect.acsType:
                   of acsPublic: "public:"
                   of acsPrivate: "private:"
                   of acsProtected: "protected:"

  CNode(
    code: sect_comm & acs_type,
    under: sect.body.mapIt(acn_to_cnode(it[])))


proc acn_class_to_cnode(acn: Acn): CNode =
  let parent_cnodes =
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
        CNode(code: "{")]

  let section_cnodes = acn.sections.map(cls_section_to_cnode)

  let body_cnodes = map(acn.body, acn_to_cnode)


  CNode(
    code: &"class {acn.name}",
    under: concat(
      parent_cnodes,
      body_cnodes,
      section_cnodes,
      @[CNode(code: "};\n\n")]))


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
      CNode(code: join(map(acn.args, var_to_string), ", ")),
      CNode(code: ") {\n"),
      CNode(under: map(acn.body, acn_to_cnode)),
      CNode(code: "}")
  ])

proc acn_pred_to_cnode(acn: Acn): CNode =
  CNode(code: acn.code)


proc body_to_cnodes(body: seq[Acn], closing: string = "}"): seq[CNode] =
  concat(
    map(body, acn_to_cnode),
    @[CNode(code: closing)])

proc acn_if_stmt_to_cnode(acn: Acn): CNode =
  let comm = if acn.comm == "": "" else: "\n//" & acn.comm & "\n"

  CNode(
    code: comm &
      "if ( $# ) {" % cnode_to_string(acn_pred_to_cnode(acn.cond)),
    under: body_to_cnodes(acn.body))


proc acn_else_if_stmt_to_cnode(acn: Acn): CNode =
  let comm = if acn.comm == "": "" else: "//" & acn.comm & "\n"

  CNode(
    code: comm & "else if ( $# ) {" %
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

proc acn_field_to_cnode(acn: Acn): CNode =
  CNode(code: var_to_string(acn.val) & ";")

proc acn_while_to_cnode(acn: Acn): CNode =
  CNode(
    code: "while ( $# ) {" % cnode_to_string(acn_pred_to_cnode(acn.cond)),
    under: body_to_cnodes(acn.body))

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
        of acnField: acn_field_to_cnode(acn)
        of acnWhile: acn_while_to_cnode(acn)
  ])
