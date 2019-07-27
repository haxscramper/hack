include code_types

proc make_acn_code(str: string): Acn = Acn(kind: acnCode, code: str)

# TODO throw invalid argument in the end of the function
proc make_string_to_enum(
  acn_enum: Acn,
  arg: Var = Var(
    name: "arg",
    vtyp: "const std::string&")): Acn =
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
    Var(name: "arg", vtyp: acn_enum.name)

  let switch_cases: seq[(string, Acn)] =
    map(acn_enum.eFields,
        proc(acn_field: Var): (string, Acn) =
          let switch_case: Acn = Acn(
            kind: acnCode,
            code: "return $# ;" % acn_field.name)

          return (acn_field.vtyp, switch_case))

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
