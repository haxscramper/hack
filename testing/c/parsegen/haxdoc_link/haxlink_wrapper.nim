
import
  hparse / htreesitter / htreesitter, sequtils, strutils

type
  HaxlinkNodeKind* = enum
    haxlinkArglist,         ## arglist
    haxlinkElementPath,     ## elementPath
    haxlinkMain,            ## main
    haxlinkNamespace,       ## namespace
    haxlinkPackage,         ## package
    haxlinkPlainEntry,      ## plainEntry
    haxlinkType,            ## type
    haxlinkLParTok,         ## (
    haxlinkRParTok,         ## )
    haxlinkCommaTok,        ## ,
    haxlinkDotTok,          ## .
    haxlinkSlashTok,        ## /
    haxlinkDoubleColonTok,  ## ::
    haxlinkLessThanTok,     ## <
    haxlinkGreaterThanTok,  ## >
    haxlinkLBrackTok,       ## [
    haxlinkRBrackTok,       ## ]
    haxlinkIdent,           ## ident
    haxlinkSyntaxError       ## Tree-sitter parser syntax error
type
  HaxlinkNode* = distinct TSNode
type
  HaxlinkParser* = distinct PtsParser
proc tsNodeType*(node: HaxlinkNode): string
proc kind*(node: HaxlinkNode): HaxlinkNodeKind {.noSideEffect.} =
  {.cast(noSideEffect).}:
    case node.tsNodeType
    of "arglist":
      haxlinkArglist
    of "elementPath":
      haxlinkElementPath
    of "main":
      haxlinkMain
    of "namespace":
      haxlinkNamespace
    of "package":
      haxlinkPackage
    of "plainEntry":
      haxlinkPlainEntry
    of "type":
      haxlinkType
    of "(":
      haxlinkLParTok
    of ")":
      haxlinkRParTok
    of ",":
      haxlinkCommaTok
    of ".":
      haxlinkDotTok
    of "/":
      haxlinkSlashTok
    of "::":
      haxlinkDoubleColonTok
    of "<":
      haxlinkLessThanTok
    of ">":
      haxlinkGreaterThanTok
    of "[":
      haxlinkLBrackTok
    of "]":
      haxlinkRBrackTok
    of "ident":
      haxlinkIdent
    of "ERROR":
      haxlinkSyntaxError
    else:
      raiseAssert("Invalid element name \'" & node.tsNodeType & "\'")

proc tree_sitter_haxlink(): PtsLanguage {.importc, cdecl.}
proc tsNodeType*(node: HaxlinkNode): string =
  $ts_node_type(TSNode(node))

proc newHaxlinkParser*(): HaxlinkParser =
  result = HaxlinkParser(ts_parser_new())
  discard ts_parser_set_language(PtsParser(result), tree_sitter_haxlink())

proc parseString*(parser: HaxlinkParser; str: string): HaxlinkNode =
  HaxlinkNode(ts_tree_root_node(ts_parser_parse_string(PtsParser(parser), nil,
      str.cstring, uint32(len(str)))))

proc parseHaxlinkString*(str: string): HaxlinkNode =
  let parser = newHaxlinkParser()
  return parseString(parser, str)

func `[]`*(node: HaxlinkNode; idx: int; withUnnamed: bool = false): HaxlinkNode =
  if withUnnamed:
    HaxlinkNode(ts_node_child(TSNode(node), uint32(idx)))
  else:
    HaxlinkNode(ts_node_named_child(TSNode(node), uint32(idx)))

func len*(node: HaxlinkNode; withUnnamed: bool = false): int =
  if withUnnamed:
    int(ts_node_child_count(TSNode(node)))
  else:
    int(ts_node_named_child_count(TSNode(node)))

proc isNil*(node: HaxlinkNode): bool =
  ts_node_is_null(TsNode(node))

iterator items*(node: HaxlinkNode; withUnnamed: bool = false): HaxlinkNode =
  for i in 0 .. node.len(withUnnamed):
    yield node[i, withUnnamed]

func slice*(node: HaxlinkNode): Slice[int] =
  {.cast(noSideEffect).}:
    ts_node_start_byte(TsNode(node)).int ..< ts_node_end_byte(TsNode(node)).int

proc treeRepr*(mainNode: HaxlinkNode; instr: string; withUnnamed: bool = false): string =
  proc aux(node: HaxlinkNode; level: int): seq[string] =
    if not(node.isNil()):
      result = @["  ".repeat(level) & ($node.kind())[7 ..^ 1]]
      if node.len(withUnnamed) == 0:
        result[0] &= " " & instr[node.slice()]
      for subn in items(node, withUnnamed):
        result.add subn.aux(level + 1)

  return aux(mainNode, 0).join("\n")
