import sugar, strutils, sequtils, strformat, options, sets, algorithm

func `|>`[A, B](a: A, b: A -> B): B = b(a)

func `|-`[A, B, C](f: A -> B, g: B -> C, x: A): C = g (f x)

func id[A](x: A): A = x

func swap[A, B, C](f: (A, B) -> C, b: B, a: A): C = f(a, b)

func `>>=`[A, B](a: Option[A], f: A -> B): Option[B] =
  if a.isSome(): some f(a.get()) else: none(B)

func `//`[A](ins: seq[A], pr: A -> bool): seq[A] = ins.filter(pr)
func `/@`[A, B](ins: seq[A], f: A -> B): seq[B] = ins.map(f)

template withIt*(val, body: untyped): untyped =
  block:
    var it {.inject.} = val
    block:
      body
    it

#*************************************************************************#
#*************************  Operator playground  *************************#
#*************************************************************************#

block: # THE cursed code
  discard
  # echo @[1, 2, 3
  # ] // ( # Filter
  #   (a) => (a > 2)
  # ) /@ ( # Map
  #   (a) => $a & "--"
  # )

#*************************************************************************#
#**************************  Type definitions  ***************************#
#*************************************************************************#

type
  # `C` is a terminal category. Lexeme is represented as string
  Tok[C] = Option[C] -> bool
  Input[C] = proc(pos: int): Option[C] {.noSideEffect.}
  Symbol[C] = object
    case isTerm: bool
      of true:
        terminal: tuple[ok: Tok[C], lex: string]
      of false:
        nterm: string

  NullSet = object
    nulls: HashSet[string]

  Rule[C] = object
    lhs: string
    rhs: seq[Symbol[C]]

  EItem = object
    ruleId: int
    startPos: int
    nextPos: int

  Node = int
  Edge = object
    ruleId: int
    finish: int

  Path = tuple
    node: Node
    edge: Edge

  State = seq[seq[EItem]]
  Chart = seq[seq[Edge]]

  Grammar[C] = object
    rules: seq[Rule[C]]
    start: string

  ParseTree[C] = object
    case isToken: bool
      of true:
        token: C
      of false:
        ruleId: int
        subnodes: seq[ParseTree[C]]

func contains(ns: NullSet, s: string): bool = s in ns.nulls

#*************************************************************************#
#*******************  Lising of all nullable symbols  ********************#
#*************************************************************************#

func isNullable[C](ns: NullSet, rule: Rule[C]): bool =
  for item in rule.rhs:
    if item.isTerm:
      return false
    else:
      if item.nterm notin ns:
        return false

  return true


func nullableSymbols[C](grammar: Grammar[C]): NullSet =
  discard


func hasLoops[C](grammar: Grammar[C]): bool =
  let nullable: NullSet = nullableSymbols grammar

  # func get
  # let rules = grammar.rules // (
  #   (rule: Rule[C]) => (rule.rhs /@ )
  # ) // (
  #   (rhs) => isNullable(nullable, rhs)
  # )

func ruleName[C](gr: Grammar[C], idx: int): string =
  ## Get rule name at `idx`
  gr.rules[idx].lhs

func ruleBody[C](gr: Grammar[C], idx: int): seq[Symbol[C]] =
  ## Get rhs from rule at `idx`
  gr.rules[idx].rhs

func nextSymbol[C](gr: Grammar[C], item: EItem): Option[Symbol[C]] =
  if gr.ruleBody(item.ruleId).len > item.nextPos:
    some(gr.ruleBody(item.ruleId)[item.nextPos])
  else:
    none(Symbol[C])

func append[A](a: var seq[A], b: A): void =
  for it in a:
    if it == b:
      return

  a.add b

func chartOfItems[C](gr: Grammar[C],
                     s: State): Chart =
  result = s.mapIt(newSeqWith(0, Edge()))
  for idx, itemSet in s:
    for item in itemSet:
      let sym: Option[Symbol[C]] = gr.nextSymbol(item)
      if sym.isSome():
        discard # Item not fully completed
      else:
        result[item.startPos].add Edge(
          ruleId: item.ruleId,
          finish: idx # REVIEW NOTE this is an index of itemset, not
                      # position in item itself
        )

  for edgeset in mitems(result):
    edgeset.sort do (e1, e2: Edge) -> int:
      if e1.ruleId == e2.ruleId:
        e1.finish - e2.finish # FIXME
      else:
        -(e1.ruleId - e2.ruleId) # FIXME

func ruleName[C](gr: Grammar[C], e: Edge): string =
  gr.ruleName(e.ruleId)

func optFind(f: proc(a: Edge): Option[Path],
             s: seq[Edge]): Path =
  for item in s:
    let res = f(item)
    if res.isSome():
      return res.get()

func optFindMem(f: proc(a: Edge): Option[seq[Path]],
                s: seq[Edge]): seq[(Edge, seq[Path])] =
  for item in s:
    let res = f(item)
    if res.isSome():
      result.add (item, res.get())

func dfSearch(edges: proc(startPos, start: int): seq[Edge] {.noSideEffect.},
              # child: proc(edge: Edge): Node {.noSideEffect.},
              isLeafp: proc(startPos: int, node: Node): bool {.noSideEffect.},
              root: Node
             ): seq[Path] =

  func aux(startPos: int, node: Node): seq[Path] =
    if isLeafp(startPos, node):
      return @[]
    else:
      for edge in edges(startPos, node):
        result.add aux(startPos + 1, edge.finish) # child(edge)

    # func aux2(edge: Edge): Option[seq[Path]] =
    #   some(aux(startPos + 1, child(startPos, edge)))

      #   result.add aux(edge)

      # let res: seq[(Edge, seq[Path])] = optFindMem(
      #   aux2, edges(startPos, node))

      # for (edge, path) in res:
      #   return @[(root, edge)]


  return aux(0, root)

func topList[C](gr: Grammar[C],
                input: Input[C],
                chart: Chart,
                path: Path): seq[Path] =
  let
    edge = path.edge
    start = path.node
    symbols = gr.ruleBody(edge.ruleId)
    ruleLen = symbols.len

  func isLeafp(startPos: int, node: Node): bool =
    (startPos == ruleLen) and (node == edge.finish)

  # func child(e: Edge): int = edge.finish
  func edges(startPos, start: int): seq[Edge] =
    if startPos >= symbols.len:
      @[]
    else:
      let sym: Symbol[C] = symbols[startPos]
      if sym.isTerm:
        if sym.terminal.ok(input start):
          @[Edge(finish: start + 1, ruleId: -1)]
        else:
          @[]
      else:
        chart[start].filterIt(gr.ruleName(it.ruleId) == sym.nterm)

  return dfSearch(edges, # child,
                  isLeafp, start)


func parseTree[C](gr: Grammar[C],
                  input: Input[C],
                  chart: Chart): Option[ParseTree[C]] =

  let start = 0
  let finish = chart.len - 1
  let name = gr.start
  # func aux(start: int, )
  func aux(path: Path): Option[ParseTree[C]] =
    if path.edge.ruleId == -1:
      some(ParseTree[C](
        isToken: true, token: input(path.node).get()))
    else:
      let subn: seq[ParseTree[C]] = gr.topList(input, chart, path).
        mapIt(aux it).
        filterIt(it.isSome()).
        mapIt(it.get())

      some(ParseTree[C](
        isToken: false,
        ruleId: path.edge.ruleId,
        subnodes: subn
      ))

  for edge in chart[start]:
    if edge.finish == finish and ruleName(gr, edge) == name:
      let tree = aux((start, edge))
      if tree.isSome():
        return tree


func predict[C](s: var State, # DOC
             i: int,
             j: int, # DOC
             nullable: NullSet,
             symbol: string, # FIXME ???
             gr: Grammar[C]
            ): int =
  # Prediction. The symbol at the right of the fat dot is
  # non-terminal. We add the the corresponding rules to the current
  # state set.
  for ruleId, rule in gr.rules:
    if rule.lhs == symbol:
      s[i].append(EItem(ruleId: ruleId, startPos: i, nextPos: 0))

    if symbol in nullable:
      s[i].append s[i][j].withIt do:
        inc it.nextPos

func scan[C](s: var State,
             i, j: int,
             symbol: Tok[C],
             gr: Grammar[C],
             input: int -> Option[C]): int =
  # Scan. The symbol at the right of the fat dot is terminal. We check
  # if the input matches this symbol. If it does, we add this item
  # (advanced one step) to the next state set.
  if symbol(input i):
    if s.len - 1 <= i:
      s.add @[]

    s[i + 1].add s[i][j].withIt do:
      inc it.nextPos


func complete[C](s: var State,
                 i, j: int,
                 gr: Grammar[C],
                 input: int -> Option[C]): int =
  # Completion. There is nothing at the right of the fat dot. This
  # means we have a successful partial parse. We look for the parent
  # items, and add them (advanced one step) to this state set.
  let item = s[i][j]
  for oldItem in s[item.startPos]:
    let next = gr.nextSymbol(oldItem)
    if next.isNone():
      discard
    else:
      let sym: Symbol[C] = next.get()
      if sym.isTerm:
        discard
      else:
        if sym.nterm == gr.ruleName(item.ruleId):
          s[i].append oldItem.withIt do:
            inc it.nextPos


func buildItems[C](gr: Grammar[C],
                   input: int -> Option[C]): State =
  let nullable = nullableSymbols gr
  var s: State

  # Seed s with the start symbol
  for idx, rule in gr.rules:
    if rule.lhs == gr.start:
      s.add @[EItem(ruleId: idx, startPos: 0, nextPos: 0)]

  var itemset = 0 # DOC
  while itemset < s.len: # Loop over main array of state sets
    var j = 0 # DOC
    while j < s[itemset].len: # Loop over elements in particular state set
      let next: Option[Symbol[C]] = gr.nextSymbol(s[itemset][j])
      if next.isNone():
        discard complete(s, itemset, j, gr, input)
      else:
        let sym: Symbol[C] = next.get()
        if sym.isTerm:
          discard scan(s, itemset, j, sym.terminal.ok, gr, input)
        else:
          discard predict(s, itemset, j, nullable, sym.nterm, gr)

      inc j
    inc itemset

  return s

#*************************************************************************#
#****************************  Test grammar  *****************************#
#*************************************************************************#

func lispRepr[C](pt: ParseTree[C], gr: Grammar[C]): string =
  if pt.isToken:
    $pt.token
  else:
    fmt("({gr.ruleName(pt.ruleId)}") & (
      block:
        if pt.subnodes.len > 0:
          " " & pt.subnodes.mapIt(lispRepr(it, gr)).join(" ") & ")"
        else:
          ")"
    )

func treeRepr[C](pt: ParseTree[C], gr: Grammar[C], level: int = 0): string =
  let pref = "  ".repeat(level)
  if pt.isToken:
    pref & $pt.token
  else:
    fmt("{pref}{gr.ruleName(pt.ruleId)}") & (
      block:
        if pt.subnodes.len > 0:
          "\n" & pt.subnodes.mapIt(treeRepr(it, gr, level + 1)).join("\n")
        else:
          ""
    )

proc printItems[C](gr: Grammar[C], state: State, onlyFull: bool = false): void =
  for idx, stateset in state:
    echo fmt("   === {idx:^3} ===   ")
    for item in stateset:
      if (item.nextPos == gr.ruleBody(item.ruleId).len) or (not onlyFull):
        var buf = fmt("{gr.ruleName(item.ruleId):<12}") & " ->"
        for idx, sym in gr.ruleBody(item.ruleId):
          if idx == item.nextPos:
            buf &= " •"

          if sym.isTerm:
            buf &= " " & sym.terminal.lex
          else:
            buf &= " " & sym.nterm

        if item.nextPos == gr.ruleBody(item.ruleId).len:
          buf = fmt("{buf:<60} \e[4m#\e[24m ({item.startPos})")
        else:
          buf = fmt("{buf:<60}   ({item.startPos})")

        echo buf

func rule(lhs: string, elems: seq[Symbol[char]]): Rule[char] =
  Rule[char](lhs: lhs, rhs: elems)

func n(nterm: string): Symbol[char] =
  Symbol[char](isTerm: false, nterm: nterm)

func alt(alts: string): Symbol[char] =
  # Match any char from the string

  let altsset = alts.toHashSet()
  Symbol[char](isTerm: true, terminal: (
    ok: proc(c: Option[char]): bool = c.isSome() and (c.get() in altsset),
    lex: &"[{alts}]"
  ))

func ch(ic: char): Symbol[char] =
  Symbol[char](isTerm: true, terminal: (
    ok: proc(c: Option[char]): bool = (c.get() == ic),
    lex: &"'{ic}'"
  ))

func rng(a, b: char): Symbol[char] =
  Symbol[char](isTerm: true, terminal: (
    ok: proc(c: Option[char]): bool = (c.get in {a .. b}),
    lex: &"[{a}-{b}]"
  ))

func makeInput(s: string): Input[char] =
  result = proc(pos: int): Option[char] {.noSideEffect.} =
    if pos < s.len:
      some(s[pos])
    else:
      none(char)


block:
  let grammar1 = Grammar[char](
    start: "Sum",
    rules: @[
      rule("Sum",     @[n "Sum",       alt "+-", n "Product" ]),
      rule("Sum",     @[n "Product"                          ]),
      rule("Product", @[n "Product",   alt "*/", n "Factor"  ]),
      rule("Product", @[n "Factor"                           ]),
      rule("Factor",  @[ch '(',        n "Sum",  ch ')'      ]),
      rule("Factor",  @[n "Number"                           ]),
      rule("Number",  @[n "Number",    rng('0',  '9')        ]),
      rule("Number",  @[rng('0',       '9')                  ])
    ]
  )


  let input1 = makeInput "1+(2*3+4)"
  let s1     = buildItems(grammar1, input1)
  # printItems(grammar1, s1)
  let c1     = chartOfItems(grammar1, s1)
  echo "\e[41m ========== chart ========== \e[49m"
  printItems(grammar1, s1, onlyFull = true)
  let pt1    = parseTree(grammar1, input1, c1)
  echo pt1.get().lispRepr(grammar1)
  echo pt1.get().treeRepr(grammar1)
