include code_types

proc make_const_ref_string(): Type =
  Type(kind: string_t, spec: @[const_t, ref_t])

proc make_enum_type(enum_name: string): Type =
  Type(kind: enum_t, eName: enum_name)

proc make_acn_code(str: string): Acn = Acn(kind: acnCode, code: str)

# TODO throw invalid argument in the end of the function
proc make_string_to_enum(
  acn_enum: Acn,
  arg: Var = Var(
    name: "arg",
    vtyp: make_const_ref_string())): Acn =
    Acn(
      kind: acnFunction,
      restype: acn_enum.name,
      name: "string_to_$#" % acn_enum.name,
      args: @[arg],
      body: concat(map(
        acn_enum.eFields,
        proc(field: Var): Acn =
          Acn(
            kind: acnIfStmt,
            cond: make_acn_code(
              "$# == \"$#\" " % @[arg.name, field.name]),
            body: @[make_acn_code("return $#::$# ;" % [acn_enum.name, field.name])]))))


proc make_throw_invalid_arg(message: string): Acn =
  Acn(
    kind: acnCode,
    code: "throw std:invalid_arg($#)" % message)

proc make_enum_to_string(
  acn_enum: Acn,
  res: string = "std::string"): Acn =

  let arg: Var =
    Var(
      name: "arg",
      vtyp: make_enum_type(acn_enum.name))

  let switch_cases: seq[(string, Acn)] =
    map(acn_enum.eFields,
        proc(acn_field: Var): (string, Acn) =
          let switch_case: Acn = Acn(
            kind: acnCode,
            code: "return \"$#\" ;" % acn_field.name)

          return(
            acn_enum.name & "::" & acn_field.name,
            switch_case))

  let selector_switch: Acn = Acn(
    kind: acnSwitch,
    swVar: arg,
    swCases: switch_cases,
    swDefault: make_throw_invalid_arg("Invalid enum value"))

  Acn(
    kind: acnFunction,
    restype: res,
    name: "$#_to_string" % acn_enum.name,
    args: @[arg],
    body: @[selector_switch])



proc make_enum(name: string, eFields: seq[string]): Acn =
  Acn(
    kind: acnEnum,
    name: name,
    eFields: map(eFields, proc(str: string): Var = Var(name: str)))

proc make_enum(tmp: (string, seq[string])): Acn =
  make_enum(tmp[0], tmp[1])

proc make_acn_predicate(code: string): Acn =
  Acn(kind: acnPredicate, code: code)

proc make_acn_if(cond: string, code: string): Acn =
  Acn(
    kind: acnIfStmt,
    cond: make_acn_predicate(cond),
    body: @[make_acn_code(code)])

proc make_acn_field(field_var: Var): Acn =
  Acn(kind: acnField, val: field_var)

proc to_ref[T](x: T): ref T =
  new(result); result[] = x

proc add_fields(cls: Acn, section_vars: seq[Var], access: AcsType = acsPrivate): Acn =
  Acn(
    kind: acnClass,
    name: cls.name,
    body: cls.body,
    parents: cls.parents,
    sections: concat(
      cls.sections,
      @[ClsSection(
        acsType: access,
        body:
          section_vars
          .map(make_acn_field)
          .map(to_ref))]))


proc add_section(
  cls: Acn,
  section: ClsSection,
  comm: string = nil): Acn =

  new(result); result = cls

  if comm != nil:
    var sect = section
    sect.comm = comm
    result.sections = concat(cls.sections, @[sect])
  else:
    result.sections = concat(cls.sections, @[section])



proc get_class_fields(cls: Acn): seq[(Var, AcsType)] =
  proc get_section_fields(
    sect: ClsSection): seq[(Var, AcsType)] =
      let acsType = sect.acsType
      return sect
      .body
      .filterIt(it.kind == acnField)
      .mapIt((it.val, acsType))

  cls
  .sections
  .map(get_section_fields)
  .concat()
