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
  echo @[1, 2, 3
  ] // ( # Filter
    (a) => (a > 2)
  ) /@ ( # Map
    (a) => $a & "--"
  )

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

  Path = (Node, Edge)

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
  #[ IMPLEMENT ]#
  discard

func append[A](a: var seq[A], b: A): int =
  a.add b
  return a.len

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
        e1.ruleId - e2.ruleId # FIXME

func ruleName[C](gr: Grammar[C], e: Edge): string =
  gr.ruleName(e.ruleId)

func optFind(f: proc(a: Edge): Option[Path],
             s: seq[Edge]): Path =
  for item in s:
    let res = f(item)
    if res.isSome():
      return res.get()

func optFindMem(f: proc(a: Edge): Option[seq[Path]],
                s: seq[Edge]): Edge =
  for item in s:
    let res = f(item)
    if res.isSome():
      return item

func dfSearch(edges: proc(depth, start: int): seq[Edge] {.noSideEffect.},
              child: proc(depth: int, edge: Edge): int {.noSideEffect.},
              pred: proc(depth, start: int): bool {.noSideEffect.},
              root: int
             ): Option[seq[Path]] =
  func aux0(depth, root: Node): Option[seq[Path]] =
    func aux1(e: Edge): Option[seq[Path]] =
      discard

    func aux2(edge: Edge, path: Path): Option[seq[Path]] =
      some(@[(root, edge)])

    if pred(depth, root):
      # some(@[])
      let empty: seq[Path] = @[]
      return some(empty)
    else:
      let firstEdge: Edge = optFindMem(aux1, edges(depth, root))
      # return aux2(firstEdge)

  return aux0(0, root)

func topList[C](gr: Grammar[C],
                input: Input[C],
                chart: Chart,
                start: int,
                edge: Edge): seq[tuple[start: int, edge: Edge]] =
  let symbols = gr.ruleBody(edge.ruleId)
  let bottom = symbols.len
  func pred(depth, start: int): bool =
    depth == bottom and start == edge.finish

  func child(depth: int, e: Edge): int = edge.finish
  func edges(depth, start: int): seq[Edge] =
    if depth >= symbols.len:
      @[]
    else:
      let sym: Symbol[C] = symbols[depth]
      if sym.isTerm:
        if sym.terminal.ok(input start):
          @[Edge(finish: start + 1, ruleId: -1)]
        else:
          @[]
      else:
        chart[start].filterIt(gr.ruleName(it.ruleId) == sym.nterm)

  let searchRes = dfSearch(edges, child, pred, start)
  if searchRes.isSome():
    return searchRes.get()
  else:
    raiseAssert("EEEEE")


func parseTree[C](gr: Grammar[C],
                  input: Input[C],
                  chart: Chart): Option[ParseTree[C]] =

  let start = 0
  let finish = chart.len - 1
  let name = gr.start
  # func aux(start: int, )
  func aux(arg: tuple[start: int, edge: Edge]): ParseTree[C] =
    {.noSideEffect.}:
      if arg.edge.ruleId == -1:
        ParseTree[C](isToken: true, token: input(arg.start).get())
      else:
        ParseTree[C](
          isToken: false,
          ruleId: arg.edge.ruleId,
          subnodes: gr.topList(input, chart, arg.start, arg.edge).map(aux)
        )



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
  for k, rule in gr.rules:
    if rule.lhs == symbol:
      s[i].add(EItem(ruleId: k, startPos: i, nextPos: 0))

  if symbol in nullable:
    var curr: EItem = s[i][j]
    inc curr.nextPos
    s[i].add(curr)

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
  let completeItem = s[i][j]
  for item in s[completeItem.startPos]:
    let next = gr.nextSymbol(item)
    if next.isNone():
      discard
    else:
      let sym: Symbol[C] = next.get()
      if sym.isTerm:
        discard
      else:
        if sym.nterm == gr.ruleName(completeItem.ruleId):
          s[i].add item.withIt do:
            inc it.nextPos


func buildItems[C](gr: Grammar[C],
                   input: int -> Option[C]): State =
  let nullable = nullableSymbols gr
  var s: State

  # Seed s with the start symbol
  for idx, rule in gr.rules:
    if rule.lhs == gr.start:
      s.add @[EItem(ruleId: idx, startPos: 0, nextPos: 0)]

  var i = 0 # DOC
  while i < s.len: # Loop over main array of state sets
    var j = 0 # DOC
    while j < s[i].len: # Loop over elements in particular state set
      let next: Option[Symbol[C]] = gr.nextSymbol(s[i][j])
      if next.isNone():
        discard complete(s, i, j, gr, input)
      else:
        let sym: Symbol[C] = next.get()
        if sym.isTerm:
          discard predict(s, i, j, nullable, sym.nterm, gr)
        else:
          discard scan(s, i, j, sym.terminal.ok, gr, input)

      inc j
    inc i

  return s

#*************************************************************************#
#****************************  Test grammar  *****************************#
#*************************************************************************#

func rule(lhs: string, elems: seq[Symbol[char]]): Rule[char] =
  Rule[char](lhs: lhs, rhs: elems)

func n(nterm: string): Symbol[char] =
  Symbol[char](isTerm: false, nterm: nterm)

func alt(alts: string): Symbol[char] =
  # Match any char from the string

  let alts = alts.toHashSet()
  Symbol[char](isTerm: true, terminal: (
    ok: proc(c: Option[char]): bool = (c.get() in alts),
    lex: ""
  ))

func ch(ic: char): Symbol[char] =
  Symbol[char](isTerm: true, terminal: (
    ok: proc(c: Option[char]): bool = (c.get() == ic),
    lex: ""
  ))

func rng(a, b: char): Symbol[char] =
  Symbol[char](isTerm: true, terminal: (
    ok: proc(c: Option[char]): bool = (c.get in {a .. b}),
    lex: ""
  ))

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

func makeInput(s: string): Input[char] =
  result = proc(pos: int): Option[char] {.noSideEffect.} =
    if pos < s.len:
      some(s[pos])
    else:
      none(char)

let input1 = makeInput "1+(2*3+4)"
let s1  = buildItems(grammar1, input1)
let c1  = chartOfItems(grammar1, s1)
let pt1 = parseTree(grammar1, input1, c1)
