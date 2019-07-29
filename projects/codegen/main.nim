import strformat
import sequtils
import strutils
import osproc

include acn_to_cpp_cnode

proc make_acn_else_if(cond: string, body: seq[Acn]): Acn =
  Acn(
    kind: acnElseIfStmt,
    body: body,
    cond: make_acn_predicate(cond))

proc make_acn_if(cond: string, body: Acn): Acn =
  Acn(
    kind: acnIfStmt,
    body: @[body],
    cond: make_acn_predicate(cond))


proc make_acn_else(body: string): Acn =
  Acn(kind: acnElseStmt, body: @[make_acn_code(body)])

proc make_acn_while(cond: string, body: seq[Acn]): Acn =
  Acn(
    kind: acnWhile,
    body: body,
    cond: make_acn_predicate(cond))

proc make_class_ptr_type(cls: Acn): Type =
  Type(kind: other_t, oName: cls.name, spec: @[ptr_t])

proc make_ptr_type(type_name: string): Type =
  Type(kind: other_t, oName: type_name, spec: @[ptr_t])

## Generate code for parsing QXmlStreamReader into instance of the
## class
proc acn_class_to_xml_reader(cls: Acn): Acn =
  echo "=== acn_class_to_xml_reader"
  let func_name = "read" & cls.name & "XML"
  let restype = "void"
  let args = @[
    Var(
      name: "target",
      vtyp: make_class_ptr_type(cls)),
    Var(
      name: "xmlStream",
      vtyp: make_ptr_type("QXmlStreamReader")),
    Var(
      name: "_tags",
      vtyp: make_ptr_type("void"))
  ]

  let class_fields: seq[(Var, AcsType)] = cls.get_class_fields()


  echo class_fields.mapIt(it[0].name).join("\n")

  let stream_name = "xmlStream->name()"

  iterator enumerate[T](s: seq[T]): (int, T) =
    var i = 0
    while i < len(s):
      yield (i, s[i])
      i += 1

  let else_skip = make_acn_else("xmlStream->skipCurrentElement();")

  iterator make_field_reader(clFields: seq[Var]): Acn {.inline.} =
    for i in 0..<len(clFields):
      let field: Var = clFields[i]
      let cond = stream_name &
        " == tags->" &
        cls.name.toLowerAscii &
        "." &
        field.name

      let code = make_acn_code("taget->set" &
        field.name.capitalizeAscii &
        "();")

      if i == 0:
        yield make_acn_if(cond, code)
      else:
        yield make_acn_else_if(cond, @[code])

    yield else_skip

  let read_next_elem = "xmlStream->readNextStartElement()"
  let field_readers = toSeq(make_field_reader(class_fields.mapIt(it[0])))
  let tags_class = cls.name & "::" & cls.name & "XMLTags"
  let field_read_while = make_acn_while(read_next_elem, field_readers)

  let base_selector_while = make_acn_while(
    read_next_elem,
    @[make_acn_if(
      stream_name & " == tags->base.section",
      "\n// read base metadata xml"),
      make_acn_else_if(
        stream_name &
          " ==  tags->" &
          cls.name.toLowerAscii &
          ".section",
        @[field_read_while]),
      else_skip])

  let body = @[
    make_acn_code(tags_class & "* tags;"),
    make_acn_if("_tags == nullptr", "tags = &target->xmlTags;"),
    make_acn_else("tags = static_cast<" & tags_class & "*>(_tags)"),
    make_acn_code(""),
    base_selector_while]

  defer:
    echo "=== ###"

  return Acn(
    kind: acnFunction,
    name: func_name,
    args: args,
    restype: restype,
    body: body)


proc print_acn_tree(acn: Acn, level: int = 0) =
  let prefix = repeat(' ', level * 2)
  case acn.kind:
    of acnClass:
      echo prefix, "class ", acn.name
      for sect in acn.sections:
        echo prefix, "  ", case sect.acsType:
               of acsPublic: "public"
               of acsPrivate: "private"
               of acsProtected: "protected"

        for item in sect.body:
          print_acn_tree(item[], level + 2)

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
       acn.args.mapIt("[ " & type_to_string(it.vtyp) & " ]").join(" X "),
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
    of acnField:
      echo prefix, acn.val.name, ": ", type_to_string(acn.val.vtyp)
    else:
      echo prefix, repr(acn.kind)

  for node in acn.body:
    print_acn_tree(node, level + 1)



var file = open("parse.cpp", fmWrite)
let enum_specs: seq[(string, seq[string])] =
  @[("Status", @["NoStatus", "Completed"])]

proc make_enum_type(acn_enum: Acn): Type =
  Type(kind: enum_t, eName: acn_enum.name)

let enum_fields = ClsSection(
  acsType: acsPrivate,
  body: enum_specs
    .mapIt(Var(
      name: it[0][0].toLowerAscii() & it[0][1..^1],
      vtyp: make_enum_type(it[0])))
    .map(make_acn_field)
    .map(to_ref))

let acn_enums = map(enum_specs, make_enum)

proc make_int_t(): Type =
  Type(kind: int_t)

let class_test = Acn(
  kind: acnClass,
  name: "QSTodo"
).add_fields(
  @[
    Var(name: "weight1", vtyp: make_int_t()),
    Var(name: "weight2", vtyp: make_int_t()),
    Var(name: "weight3", vtyp: make_int_t()),
  ]
).add_section(
  section = enum_fields,
  comm = "enum fields"
)

let xml_converter = acn_class_to_xml_reader(class_test)

#print_acn_tree(class_test)

write(file, (cnode_to_string(acn_to_cnode(class_test))))
write(file, (cnode_to_string(acn_to_cnode(xml_converter))))

close(file)
