
import
  hparse / htreesitter / htreesitter, sequtils, strutils

type
  Hts_wrapgen_1NodeKind* = enum
    hts_wrapgen_1Block,     ## block
    hts_wrapgen_1FunctionDefinition, ## function_definition
    hts_wrapgen_1ParameterList, ## parameter_list
    hts_wrapgen_1ReturnStatement, ## return_statement
    hts_wrapgen_1SourceFile, ## source_file
    hts_wrapgen_1LParTok,   ## (
    hts_wrapgen_1RParTok,   ## )
    hts_wrapgen_1SemicolonTok, ## ;
    hts_wrapgen_1BoolTok,   ## bool
    hts_wrapgen_1Comment,   ## comment
    hts_wrapgen_1FuncTok,   ## func
    hts_wrapgen_1Identifier, ## identifier
    hts_wrapgen_1Number,    ## number
    hts_wrapgen_1ReturnTok, ## return
    hts_wrapgen_1Space,     ## space
    hts_wrapgen_1LCurlyTok, ## {
    hts_wrapgen_1RCurlyTok, ## }
    hts_wrapgen_1Comment2,  ## comment
    hts_wrapgen_1Space2,    ## space
    hts_wrapgen_1SyntaxError ## Tree-sitter parser syntax error
type
  Hts_wrapgen_1ExternalTok* = enum
    hts_wrapgen_1ExternComment, ## comment
    hts_wrapgen_1ExternSpace ## space
type
  Hts_wrapgen_1Node* = distinct TSNode
type
  Hts_wrapgen_1Parser* = distinct PtsParser
proc tsNodeType*(node: Hts_wrapgen_1Node): string
proc kind*(node: Hts_wrapgen_1Node): Hts_wrapgen_1NodeKind {.noSideEffect.} =
  {.cast(noSideEffect).}:
    case node.tsNodeType
    of "block":
      hts_wrapgen_1Block
    of "function_definition":
      hts_wrapgen_1FunctionDefinition
    of "parameter_list":
      hts_wrapgen_1ParameterList
    of "return_statement":
      hts_wrapgen_1ReturnStatement
    of "source_file":
      hts_wrapgen_1SourceFile
    of "(":
      hts_wrapgen_1LParTok
    of ")":
      hts_wrapgen_1RParTok
    of ";":
      hts_wrapgen_1SemicolonTok
    of "bool":
      hts_wrapgen_1BoolTok
    of "comment":
      hts_wrapgen_1Comment2
    of "func":
      hts_wrapgen_1FuncTok
    of "identifier":
      hts_wrapgen_1Identifier
    of "number":
      hts_wrapgen_1Number
    of "return":
      hts_wrapgen_1ReturnTok
    of "space":
      hts_wrapgen_1Space2
    of "{":
      hts_wrapgen_1LCurlyTok
    of "}":
      hts_wrapgen_1RCurlyTok
    of "ERROR":
      hts_wrapgen_1SyntaxError
    else:
      raiseAssert("Invalid element name \'" & node.tsNodeType & "\'")

proc tree_sitter_hts_wrapgen_1(): PtsLanguage {.importc, cdecl.}
proc tsNodeType*(node: Hts_wrapgen_1Node): string =
  $ts_node_type(TSNode(node))

proc newHts_wrapgen_1Parser*(): Hts_wrapgen_1Parser =
  result = Hts_wrapgen_1Parser(ts_parser_new())
  discard ts_parser_set_language(PtsParser(result), tree_sitter_hts_wrapgen_1())

proc parseString*(parser: Hts_wrapgen_1Parser; str: string): Hts_wrapgen_1Node =
  Hts_wrapgen_1Node(ts_tree_root_node(ts_parser_parse_string(PtsParser(parser),
      nil, str.cstring, uint32(len(str)))))

proc parseHts_wrapgen_1String*(str: string): Hts_wrapgen_1Node =
  let parser = newHts_wrapgen_1Parser()
  return parseString(parser, str)

func `[]`*(node: Hts_wrapgen_1Node; idx: int; withUnnamed: bool = false): Hts_wrapgen_1Node =
  if withUnnamed:
    Hts_wrapgen_1Node(ts_node_child(TSNode(node), uint32(idx)))
  else:
    Hts_wrapgen_1Node(ts_node_named_child(TSNode(node), uint32(idx)))

func len*(node: Hts_wrapgen_1Node; withUnnamed: bool = false): int =
  if withUnnamed:
    int(ts_node_child_count(TSNode(node)))
  else:
    int(ts_node_named_child_count(TSNode(node)))

proc isNil*(node: Hts_wrapgen_1Node): bool =
  ts_node_is_null(TsNode(node))

iterator items*(node: Hts_wrapgen_1Node; withUnnamed: bool = false): Hts_wrapgen_1Node =
  for i in 0 .. node.len(withUnnamed):
    yield node[i, withUnnamed]

func slice*(node: Hts_wrapgen_1Node): Slice[int] =
  {.cast(noSideEffect).}:
    ts_node_start_byte(TsNode(node)).int ..< ts_node_end_byte(TsNode(node)).int

proc treeRepr*(mainNode: Hts_wrapgen_1Node; instr: string;
               withUnnamed: bool = false): string =
  proc aux(node: Hts_wrapgen_1Node; level: int): seq[string] =
    if not(node.isNil()):
      result = @["  ".repeat(level) & ($node.kind())[13 ..^ 1]]
      if node.len(withUnnamed) == 0:
        result[0] &= " " & instr[node.slice()]
      for subn in items(node, withUnnamed):
        result.add subn.aux(level + 1)

  return aux(mainNode, 0).join("\n")
