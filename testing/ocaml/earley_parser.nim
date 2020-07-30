import sugar, strutils, sequtils, strformat, options, sets, algorithm, hashes, tables

template withIt*(val, body: untyped): untyped =
  block:
    var it {.inject.} = val
    block:
      body
    it

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
    ranges: seq[tuple[start, finish: int]]

  SSetId = int # 'The nodes are the state sets. One of them is the
               # root (the search starts there), and one of them is
               # the leaf (the search stops there).'

  SItemId = object # 'The edges are the items themselves. They start
                   # from a node (indeed, they are stored in a node,
                   # and they point to another node (wherever they
                   # finish).'
    ruleId: int
    finish: int

  Path = tuple
    sset: SSetId
    ssetItem: SItemId

  State = seq[seq[EItem]]
  Chart = seq[seq[SItemId]]

  Grammar[C] = object
    rules: seq[Rule[C]]
    start: string

  ParseTree[C] = object
    start, finish: int
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
  result = s.mapIt(newSeqWith(0, SItemId()))
  for idx, itemSet in s:
    for item in itemSet:
      let sym: Option[Symbol[C]] = gr.nextSymbol(item)
      if sym.isSome():
        discard # Item not fully completed
      else:
        result[item.startPos].add SItemId(
          ruleId: item.ruleId,
          finish: idx # REVIEW NOTE this is an index of itemset, not
                      # position in item itself
        )

  for edgeset in mitems(result):
    edgeset.sort do (e2, e1: SItemId) -> int:
      if e1.ruleId == e2.ruleId:
        e2.finish - e1.finish # FIXME
      else:
        e2.ruleId - e1.ruleId # FIXME

func ruleName[C](gr: Grammar[C], e: SItemId): string =
  gr.ruleName(e.ruleId)

func hash(id: SItemId): Hash =
  var h: Hash = 0
  h = h !& id.ruleId !& id.finish
  result = !$h

type
  TryParams = object
    start: int
    altId: int
    name: string

func hash(pr: TryParams): Hash = !$(pr.start !& pr.altId !& hash(pr.name))

func join(sep: string, s: seq[string]): string = s.join(sep)

func exprRepr[C](rule: seq[Symbol[C]]): string =
  join " ", rule.mapIt do:
    if it.isTerm:
      it.terminal.lex
    else:
      it.nterm

func treeRepr[C](pt: ParseTree[C], gr: Grammar[C], level: int = 0): string =
  if level == 0:
    result &= &"\e[36m[ ] {\"=\".repeat(36)} [   ]\e[39m\n"

  let pref = "  ".repeat(level)
  if pt.isToken:
    let str = &"[*]{pref}\e[32m{pt.token}\e[39m"
    result &= fmt("{str:<50} \e[2m({pt.start}:{pt.finish})\e[22m")
   # "[*]" & pref & $pt.token &
  else:
    let str = &"[{pt.subnodes.len}] {pref}\e[33m{gr.ruleName(pt.ruleId)}.{pt.ruleId}\e[39m"
    result &= fmt("{str:<50} \e[31m[{pt.start}:{pt.finish}]\e[39m") & (
      block:
        if pt.subnodes.len > 0:
          "\n" & pt.subnodes.mapIt(treeRepr(it, gr, level + 1)).join("\n")
        else:
          ""
    )


proc parseTree[C](gr: Grammar[C],
                  input: Input[C],
                  chart: Chart): seq[ParseTree[C]] =

  var triedTable: Table[TryParams, seq[ParseTree[C]]]
  proc aux(start, finish: int, name: string, level: int): seq[ParseTree[C]] =
    ## Search for parse tree of nonterminal `name`, starting at
    ## `start` and ending at `finish` position.
    let pref = &"{level:<2}|   " & "  ".repeat(level)
    template pp(body: untyped): untyped = echo pref, fmt(body)
    template pl(body: untyped): untyped =
      if false: echo pref, fmt(body)
    template p0(body: untyped): untyped =
      echo &"{level:<2}| ", fmt(body)


    pl "Parsing rule {name} starting from {start}, search for alts"
    let alts: seq[int] = deduplicate do: collect(newSeq):
      var altsTmp: seq[int]
      for rule in chart[start]:
        let params = TryParams(start: start, altId: rule.ruleId, name: name)
        if (params in triedTable) and (rule.ruleId notin altsTmp):
          altsTmp.add rule.ruleId
          let str = &"\e[4m{name}.{rule.ruleId} ::= {ruleBody(gr, rule.ruleId).exprRepr()}\e[24m"
          pp "Already tried {str} @ \e[35m{start}\e[39m - generated {triedTable[params].len} trees"
          result.add triedTable[params]

        if (ruleName(gr, rule) == name) and (params notin triedTable):
          rule.ruleId

    if alts.len == 0:
      pp "No untried alternatives for {name} ({start}:{finish})"
    else:
      pl "There are {alts.len} rules never tried in this configuration"

    for alt in alts:
      let params = TryParams(start: start, altId: alt, name: name)
      triedTable[params] = @[]
      let str = &"\e[4m{name}.{alt} ::= {ruleBody(gr, alt).exprRepr()}\e[24m"
      pp "Trying {str} [alts: {alts}] from \e[35m{start}\e[39m"

      let symbols = gr.ruleBody(alt)
      let singletok = (symbols.len == 1) and (symbols[0].isTerm)
      if not singletok:
        result.add ParseTree[C](
          isToken: false,
          ruleId: alt,
          subnodes: @[],
          start: start
        )
      else:
        if symbols[0].terminal.ok input(start):
          result.add ParseTree[C](
            isToken: true,
            start: start,
            finish: start + 1,
            token: input(start).get()
          )
        else:
          return @[]

      for idx, sym in gr.ruleBody(alt):
        for treeIdx, tree in result:
          let begin =
            if tree.finish == 0:
              start
            else:
              tree.finish

          if sym.isTerm:
            pp "Expecting token \e[93m{sym.terminal.lex}\e[39m @ \e[35m{begin}\e[39m"
            let inp = input begin
            if sym.terminal.ok(inp):
              let str =  "  Matched" & (if singletok: " single" else: "") &
                &" token \e[32m{sym.terminal.lex}\e[39m " &
                         &"as '\e[32m{inp.get()}\e[39m'"

              pl "{str}"

              let tree = ParseTree[C](
                isToken: true,
                start: begin,
                finish: begin + 1,
                token: inp.get()
              )

              if singletok:
                result.add tree
              else:
                result[treeIdx].subnodes.add tree
                result[treeIdx].finish = begin + 1
                pp "  Advancing tree {treeIdx} to {result[treeIdx].finish}"
            else:
              pp "  Failed token match \e[31m{sym.terminal.lex}\e[39m"
          else:
            pp "Expecting \e[93m{sym.nterm}\e[39m starting at {begin}, #{idx} "
            pp "Working with {result.len} trees"
            # let currpos = tree.finish
            let res: seq[ParseTree[C]] = aux(begin, finish, sym.nterm, level + 1)
            if res.len != 0:
              pp "  Recognized rule \e[32m{name}\e[39m - {res.len} trees"
              let lastr = result[treeIdx]
              result.del treeIdx
              for tree in res:
                pp "    Tree ({tree.start}:{tree.finish})"
                result.add lastr.withIt do:
                  it.subnodes.add tree
                  it.finish = tree.finish

              # result[^1].subnodes.add res[0] # XXXX
              # result[^1].finish = currpos
              # p0 "@ \e[35m{currpos}\e[39m"
            else:
              pp "No parse trees for rule \e[31m{name}\e[39m"
              result.del treeIdx

      triedTable[params].add result
        # return # Immediately returning first matched parse tree

  for ssetItem in chart[0]: # For all items in stateset[0]
    if ssetItem.finish == (chart.len - 1) and # If item is finished
       ruleName(gr, ssetItem) == gr.start: # And it's name is equal to
                                           # grammar start name
      let tree = aux(0, chart.len - 1, gr.start, 0) # Recognize first
                                                 # possible parse tree
      # return tree.filterIt(it.finish == chart.len - 1)
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
  var s: State = @[]
  s.add @[]

  # Seed s with the start symbol
  for idx, rule in gr.rules:
    if rule.lhs == gr.start:
      # debugecho rule
      s[0].add EItem(ruleId: idx, startPos: 0, nextPos: 0)

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


proc printChart[C](gr: Grammar[C], state: Chart): void =
  echo "\e[31mCHART :\e[39m"
  for idx, stateset in state:
    echo fmt("\e[36mSTARTS:\e[39m {idx}")
    for item in stateset:
      var buf = fmt("{gr.ruleName(item.ruleId):<12}") & " ->"
      for idx, sym in gr.ruleBody(item.ruleId):
        if sym.isTerm:
          buf &= fmt(" {sym.terminal.lex:>8}")
        else:
          buf &= fmt(" {sym.nterm:>8}")

      buf = fmt("\e[32mEND   :\e[39m {item.finish} {buf:<60}")

      echo buf
    echo ""

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
    ok: proc(c: Option[char]): bool = c.isSome() and (c.get() == ic),
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


if true:
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


  let input1 = makeInput "1+(2*3-4)"
  let s1     = buildItems(grammar1, input1)
  let c1     = chartOfItems(grammar1, s1)
  printChart(grammar1, c1)
  let pt1    = parseTree(grammar1, input1, c1)
  for tree in pt1:
    echo tree.treeRepr(grammar1)


if true:
  let grammar1 = Grammar[char](
    start: "Block",
    rules: @[
      rule("Block", @[ch ';']),
      rule("Block", @[n "If"]),
      rule("If",    @[ch 'i',   n "Block"]),
      rule("If",    @[ch 'i',   n "Block",   ch 'e', n "Block"])
    ]
  )


  let input1 = makeInput "ii;e;"
  let s1     = buildItems(grammar1, input1)
  # printItems(grammar1, s1)
  let c1     = chartOfItems(grammar1, s1)
  printChart(grammar1, c1)
  let pt1    = parseTree(grammar1, input1, c1)
  for tree in pt1:
    echo tree.treeRepr(grammar1)
