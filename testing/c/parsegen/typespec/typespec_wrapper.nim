
import
  hparse / htreesitter / htreesitter, sequtils, strutils

type
  TypespecNodeKind* = enum
    typespecMain,           ## main
    typespecType,           ## type
    typespecCommaTok,       ## ,
    typespecLBrackTok,      ## [
    typespecRBrackTok,      ## ]
    typespecIdent,          ## ident
    typespecSyntaxError      ## Tree-sitter parser syntax error
type
  TypespecNode* = distinct TSNode
type
  TypespecParser* = distinct PtsParser
proc tsNodeType*(node: TypespecNode): string
proc kind*(node: TypespecNode): TypespecNodeKind {.noSideEffect.} =
  {.cast(noSideEffect).}:
    case node.tsNodeType
    of "main":
      typespecMain
    of "type":
      typespecType
    of ",":
      typespecCommaTok
    of "[":
      typespecLBrackTok
    of "]":
      typespecRBrackTok
    of "ident":
      typespecIdent
    of "ERROR":
      typespecSyntaxError
    else:
      raiseAssert("Invalid element name \'" & node.tsNodeType & "\'")

proc tree_sitter_typespec(): PtsLanguage {.importc, cdecl.}
proc tsNodeType*(node: TypespecNode): string =
  $ts_node_type(TSNode(node))

proc newTypespecParser*(): TypespecParser =
  result = TypespecParser(ts_parser_new())
  discard ts_parser_set_language(PtsParser(result), tree_sitter_typespec())

proc parseString*(parser: TypespecParser; str: string): TypespecNode =
  TypespecNode(ts_tree_root_node(ts_parser_parse_string(PtsParser(parser), nil,
      str.cstring, uint32(len(str)))))

proc parseTypespecString*(str: string): TypespecNode =
  let parser = newTypespecParser()
  return parseString(parser, str)

func `[]`*(node: TypespecNode; idx: int; withUnnamed: bool = false): TypespecNode =
  if withUnnamed:
    TypespecNode(ts_node_child(TSNode(node), uint32(idx)))
  else:
    TypespecNode(ts_node_named_child(TSNode(node), uint32(idx)))

func len*(node: TypespecNode; withUnnamed: bool = false): int =
  if withUnnamed:
    int(ts_node_child_count(TSNode(node)))
  else:
    int(ts_node_named_child_count(TSNode(node)))

proc isNil*(node: TypespecNode): bool =
  ts_node_is_null(TsNode(node))

iterator items*(node: TypespecNode; withUnnamed: bool = false): TypespecNode =
  for i in 0 .. node.len(withUnnamed):
    yield node[i, withUnnamed]

func slice*(node: TypespecNode): Slice[int] =
  {.cast(noSideEffect).}:
    ts_node_start_byte(TsNode(node)).int ..< ts_node_end_byte(TsNode(node)).int

proc treeRepr*(mainNode: TypespecNode; instr: string; withUnnamed: bool = false): string =
  proc aux(node: TypespecNode; level: int): seq[string] =
    if not(node.isNil()):
      result = @["  ".repeat(level) & ($node.kind())[8 ..^ 1]]
      if node.len(withUnnamed) == 0:
        result[0] &= " " & instr[node.slice()]
      for subn in items(node, withUnnamed):
        result.add subn.aux(level + 1)

  return aux(mainNode, 0).join("\n")
