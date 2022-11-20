import hmisc/hasts/json_serde
import hmisc/core/all
import hmisc/other/hpprint

startHax()

import std/re
import std/options
import std/sequtils
import std/httpclient
import std/json
import std/uri
import std/[strutils, parseutils]

# pprint parseUri("https://nim-lang.org/")

var client = newHttpClient()

type
  NLPResult = object
    sentences: seq[Sentence]

  Parse = ref object
    tag: string
    lexem: string
    nested: seq[Parse]

  SenDep = object
    tree: SenTree
    kind: string
    subkind: string

  SenTree = ref object
    index: int ## Index of the token in the resulting tree. Valid only if
               ## `nested.len() == 0`, otherwise defaults to 0.

    parent: SenTree
    dependency: seq[SenDep]
    governor: SenDep
    tag: string
    lexem: string
    nested: seq[SenTree]

  Dependency = object
    dep: string
    governor: int
    governorGloss: string
    dependent: int
    dependentGloss: string

  Sentence = object
    index: int
    parse: Parse
    basicDependencies: seq[Dependency]
    enhancedDependencies: seq[Dependency]
    enhancedPlusPlusDependencies: seq[Dependency]
    tokens: seq[Token]

  Token = object
    index: int
    word: string
    originaltext: string
    characterOffsetBegin: int
    characterOffsetend: int
    pos: string
    before: string
    after: string


  Lexer = object
    data: string
    pos: int

func tok(l: Lexer): char = l.data[l.pos]
func next(l: var Lexer) = inc l.pos
func at(l: Lexer, c: char): bool = l.tok() == c
func space(l: var Lexer) =
  while l.tok() in Whitespace:
    l.next()

func skip(l: var Lexer, c: char) =
  assert l.tok() == c
  l.next()

proc parseParse(lex: var Lexer): Parse =
  result = Parse()
  lex.space()
  lex.skip('(')
  while (lex.tok() in IdentChars + {'.'} or lex.tok() == '-'):
    result.tag.add(lex.tok())
    lex.next()

  lex.space()

  if (lex.at('(')):
    while (not lex.at(')')):
      lex.space();
      result.nested.add(parseParse(lex))

  else:
    while (not lex.at(')')):
      result.lexem.add(lex.tok())
      lex.next()

  lex.skip(')')


proc loadJson(reader: var JsonDeserializer, res: var Parse) =
  var text: string
  loadJson(reader, text)
  var lexer = Lexer(data: text)
  res = parseParse(lexer)

proc initNLPUrl(): URI =
  result = initUri()
  result.hostname = "localhost"
  result.port = "9000"
  result.scheme = "http"


proc getNLP(text: string): NLPResult =
  let url = initNLPUrl() ? {
    "properties": $(%*{
      "annotators": "natlog, tokenize, ssplit, pos, parse",
      "outputFormat": "json"
    })
  }

  let nlp = client.postContent($url, body = text)
  result = fromJson(nlp, NLPResult)

type
  NLPSemgrex = object

proc getSemgrex(text: string, pattern: string): NLPSemgrex =
  # TODO figure out query and failure reasons
  let url = initNLPUrl() / "semgrex" ? {
    "pattern": pattern
  }
  echo url
  let nlp = client.postContent($url, body = text)
  echo nlp

type
  Match = object
    context: Context
    contextForSurematch: int
    length: int
    message: string
    offset: int
    replacements: seq[string]
    rule: Rule
    shortMessage: string

  Rule = object
    id: string
    issueType: string
    description: string

  Category = object
    id: string
    name: string

  Context = object
    length: int
    offset: int
    text: string

  LangTool = object
    matches: seq[Match]

proc getLangtool(text: seq[tuple[markup: bool, text: string]]): LangTool =
  var url = initUri()
  url.scheme = "http"
  url.hostname = "localhost"
  url.port = "8081"
  url.path = "v2/check"

  var data = "data=$#&language=$#&enabledOnly=$#" % [
    encodeURL($(%*{
      "annotation": %text.mapIt(
        if it[0]:
          %*{ "markup": it[1] }
        else:
          %*{ "text": it[1] }
      )
    }), usePlus = false),
    "en-US",
    "false"
  ]

  let lang = client.postContent($url, body = data)
  result = fromJson(lang, LangTool)

proc asDep(it: SenDep): string =
  "$#$# -> $#" % [
    $it.kind,
    tern(it.subkind.empty(), "", ":" & it.subkind),
    $it.tree.index
  ]

proc asGovern(it: SenDep): string =
  "$#$# <- '$#'" % [
    $it.kind,
    tern(it.subkind.empty(), "", ":" & it.subkind),
    $it.tree.lexem
  ]

proc treeRepr(node: SenTree, indent: int = 0): string =
  result = repeat("  ", indent)
  let
    dep = mapIt(node.dependency, asDep(it)).join(" ")
    gov = tern(node.governor.tree.isNil(), "", asGovern(node.governor))

  result.add "$#$#$#$#" % [
    node.tag,
    if node.nested.empty(): " '$#'" % node.lexem else: "",
    if node.nested.empty(): " " & $node.index else: "",
    tern(gov.empty(), "", " " & gov)
    # tern(dep.empty(), "", " " & dep)
  ]

  for sub in node.nested:
    result.add("\n")
    result.add(sub.treeRepr(indent + 1))


proc lispRepr(node: SenTree, indent: int = 0): string =
  result.add "($#$#$#" % [
    node.tag,
    if node.nested.empty(): " '$#'" % node.lexem else: "",
    if node.nested.empty(): " " & $node.index else: ""
  ]

  for sub in node.nested:
    result.add(" ")
    result.add(sub.lispRepr(indent + 1))

  result.add(")")


proc toSenTree(nlp: NLPResult, sentence: int): SenTree =
  var tokens: seq[SenTree]
  proc aux(node: Parse, parent: SenTree): SenTree =
    result = SenTree(
      parent: parent,
      tag: node.tag,
      lexem: node.lexem
    )

    if node.nested.empty():
      tokens.add result
      result.index = tokens.high()

    else:
      for sub in node.nested:
        result.nested.add(aux(sub, result))

  let sent = nlp.sentences[sentence]
  result = aux(sent.parse, nil)

  for dep in sent.enhancedPlusPlusDependencies:
    let kind = dep.dep.split(":")

    var sendep = SenDep(
      kind: kind[0],
      subkind: tern(1 < kind.len(), kind[1], ""),
      tree: tokens[dep.dependent - 1]
    )

    if dep.dep != "ROOT":
      tokens[dep.governor - 1].dependency.add sendep
      sendep.tree = tokens[dep.governor - 1]
      tokens[dep.dependent - 1].governor = sendep

proc isLeaf(node: SenTree): bool = node.nested.len() == 0

type
  SemgrexRelationKind = enum
    SRelDependentDirect ## ``A <reln B ``: A is the dependent of a relation
                      ## reln with B
    SRelGovernorDirect ## ``A >reln B ``: A is the governor of a relation
                     ## reln with B
    SRelDependentIndirect ## ``A <<reln B ``: A is the dependent of a
                        ## relation reln in a chain to B following dep->gov
                        ## paths
    SGovernorIndirect ## ``A >>reln B ``: A is the governor of a relation
                      ## reln in a chain to B following gov->dep paths
    SRelDependentChain ## ``A x,y<<reln B ``: A is the dependent of a
                     ## relation reln in a chain to B following dep->gov
                     ## paths between distances of x and y
    SRelGovernorChain ## ``A x,y>>reln B ``: A is the governor of a relation
                    ## reln in a chain to B following gov->dep paths
                    ## between distances of x and y
    SRelSame ## ``A == B ``: A and B are the same nodes in the same graph
    SRelPrecedes ## ``A . B ``: A immediately precedes B, i.e. A.index() ==
               ## B.index() - 1
    SRelRightImmediate ## ``A $+ B ``: B is a right immediate sibling of A,
                     ## i.e. A and B have the same parent and A.index() ==
                     ## B.index() - 1
    SRelLeftImmedite ## ``A $- B ``: B is a left immediate sibling of A, i.e.
                   ## A and B have the same parent and A.index() ==
                   ## B.index() + 1
    SRelRight ## ``A $++ B ``: B is a right sibling of A, i.e. A and B have
            ## the same parent and A.index() < B.index()
    SRelLeft ## ``A $-- B ``: B is a left sibling of A, i.e. A and B have the
           ## same parent and A.index() > B.index()
    SRelAligned ## ``A @ B ``: A is aligned to B (this is only used when you
              ## have two dependency graphs which are aligned)

  SemgrexRuleKind = enum
    SRRelation
    SRMatch
    SRLogic
    SRSubtree

  SemgrexLogic = enum
    SLAnd
    SLOr
    SLOptional
    SLNot

  SemgrexTag = object
    prefix: string
    glob: bool

  SemgrexSubKind = enum
    SSDirect
    SSIndirect

  SemgrexRule = object
    case kind: SemgrexRuleKind
      of SRRelation:
        relKind: SemgrexRelationKind
        rel: seq[SemgrexRule]

      of SRMatch:
        negated: bool
        lemma: Option[Regex]
        pos: Option[SemgrexTag]
        bindto: Option[string]

      of SRLogic:
        logic: SemgrexLogic
        params: seq[SemgrexRule]

      of SRSubtree:
        subKind: SemgrexSubKind
        sub: seq[SemgrexRule]

proc `=~`(tag: string, rule: SemgrexTag): bool =
  if rule.glob:
    tag.startswith(rule.prefix)

  else:
    tag == rule.prefix


proc matches(node: SenTree, rule: SemgrexRule): bool

proc firstDirect(node: SenTree, rule: SemgrexRule): Option[SenTree] =
  ## Return first subnode of any of the specified kinds, if such node
  ## exists.
  for sub in node.nested:
    if sub.matches(rule):
      return some sub

proc firstIndirect(node: SenTree, rule: SemgrexRule): Option[SenTree] =
  for sub in node.nested:
    if sub.matches(rule):
      return some sub

    else:
      let indirect = firstIndirect(sub, rule)
      if indirect.isSome():
        return indirect

proc `$`(rule: SemgrexRule): string =
  case rule.kind:
    of SRRelation:
      assert false

    of SRMatch:
      if rule.negated:
        result.add "!"

      result.add "{"
      if rule.lemma.isSome():
        result.add("RE?" & " ")

      if rule.pos.isSome():
        result.add "/"
        result.add(
          $rule.pos.get().prefix & (
            if rule.pos.get().glob: "*" else: ""))

        result.add "/"
      result.add "}"

    of SRLogic:
      case rule.logic:
        of SLOptional:
          result = "?" & $rule.params[0]

        of SLNot:
          result = "!" & $rule.params[0]

        of SLAnd, SLOr:
          result = rule.params.mapIt($it).join(
            tern(rule.logic == SLAnd, " and ", " or ")
          )

    of SRSubtree:
      result = "$# $# $#" % [
        $rule.sub[0],
        case rule.subKind:
          of SSDirect: "->"
          of SSIndirect: "->>"
          ,
        $rule.sub[1],
      ]


proc matches(node: SenTree, rule: SemgrexRule): bool =
  case rule.kind:
    of SRRelation:
      assert false

    of SRSubtree:
      if node.matches(rule.sub[0]):
        case rule.subKind:
          of SSDirect:
            result = firstDirect(node, rule.sub[1]).isSome()

          of SSIndirect:
            result = firstIndirect(node, rule.sub[1]).isSome()

    of SRMatch:
      type MatchState = enum Matched, Failed, NotApplicable
      var
        lemma = NotApplicable
        tag = NotApplicable

      if rule.lemma.isSome():
        if node.lexem =~ rule.lemma.get():
          lemma = Matched

        else:
          lemma = Failed

      if rule.pos.isSome():
        if node.tag =~ rule.pos.get():
          tag = Matched

        else:
          tag = Failed

      result = lemma in { Matched, NotApplicable } and
               tag in { Matched, NotApplicable }

      if rule.negated:
        result = not result

    of SRLogic:
      case rule.logic:
        of SLAnd:
          result = true
          for sub in rule.params:
            if not node.matches(sub):
              return false

        of SLOr:
          result = false
          for sub in rule.params:
            if node.matches(sub):
              return true

        of SLNot:
          result = not node.matches(rule.params[0])

        of SLOptional:
          result = true

proc findMatches(node: SenTree, rule: SemgrexRule): seq[SenTree] =
  if node.matches(rule):
    result.add node

  for sub in node.nested:
    result.add findMatches(sub, rule)

func match(): SemgrexRule = SemgrexRule(kind: SRmatch)
func `not`(rule: sink SemgrexRule): SemgrexRule =
  result = rule
  result.negated = true


func rel(r1, r2: sink SemgrexRule): SemgrexRule =
  SemgrexRule(kind: SRRelation, rel: @[r1, r2])

func sub(r1, r2: sink SemgrexRule): SemgrexRule =
  SemgrexRule(kind: SRSubtree, sub: @[r1, r2])

func `->>`(r1, r2: sink SemgrexRule): SemgrexRule =
  result = sub(r1, r2)
  result.subKind = SSIndirect

func `->`(r1, r2: sink SemgrexRule): SemgrexRule =
  result = sub(r1, r2)
  result.subKind = SSDirect

func lex(rule: sink SemgrexRule, lexem: Regex): SemgrexRule =
  result = rule
  result.lemma = some lexem

func tag(rule: sink SemgrexRule, tag: string): SemgrexRule =
  result = rule
  if tag.endswith("*"):
    result.pos = some SemgrexTag(
      prefix: tag.strip(trailing = true, chars = {'*'}),
      glob: true
    )

  else:
    result.pos = some SemgrexTag(prefix: tag)

let nlp = getNLP("So the cat was stolen.")

let tree = toSenTree(nlp, 0)
echo tree.treeRepr()

let matchItems = tree.findMatches(
  match().tag("VP") ->> match().tag("VB*")
)

for it in matchItems:
  echov it.treeRepr()
