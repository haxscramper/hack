import sugar, strutils, sequtils, strformat, options, sets, algorithm, hashes, tables

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

func optFind(f: proc(a: SItemId): Option[Path],
             s: seq[SItemId]): Path =
  for item in s:
    let res = f(item)
    if res.isSome():
      return res.get()

func optFindMem(f: proc(a: SItemId): Option[seq[Path]],
                s: seq[SItemId]): seq[(SItemId, seq[Path])] =
  for item in s:
    let res = f(item)
    if res.isSome():
      result.add (item, res.get())

func dfSearch(ssetItems: proc(startPos, start: int): seq[SItemId] {.noSideEffect.},
              # child: proc(startPos: int, ssetItem: SItemId): SSetId {.noSideEffect.},
              isLeafp: proc(startPos: int, sset: SSetId): bool {.noSideEffect.},
              startSet: SSetId
             ): seq[Path] =

  func aux(startPos: int, sset: SSetId): seq[Path] =
    # func aux2(ssetItem: SItemId): Option[seq[Path]] =
    #   some(aux(startPos + 1, child(startPos, ssetItem)))

    if isLeafp(startPos, sset):
      return @[]
    else:
      # debugecho startPos, " ", sset
      for item in ssetItems(startPos, sset):
        # debugecho item
        result.add aux(startPos + 1, item.finish)

      # let res: seq[(SItemId, seq[Path])] = optFindMem(
      #   aux2, ssetItems(startPos, sset))

      # # debugecho res.len
      # # for (ssetItem, path) in res:
      # #   debugecho ssetItem, path

      # for (ssetItem, path) in res:
      #   result.add (startSet, ssetItem)


  return aux(0, startSet)

func topList[C](gr: Grammar[C],
                input: Input[C],
                chart: Chart,
                path: Path): seq[Path] =
  let
    ssetItem = path.ssetItem
    startSet = path.sset
    symbols = gr.ruleBody(ssetItem.ruleId)
    ruleLen = symbols.len

  func isLeafp(startPos: int, sset: SSetId): bool =
    (startPos == ruleLen) and (startPos == ssetItem.finish)

  # func child(startPos: int, e: SItemId): int = ssetItem.finish
  func ssetItems(startPos, start: int): seq[SItemId] =
    if startPos >= symbols.len:
      @[]
    else:
      let sym: Symbol[C] = symbols[startPos]
      if sym.isTerm:
        if sym.terminal.ok(input start):
          @[SItemId(finish: start + 1, ruleId: -1)]
        else:
          @[]
      else:
        let tmp = chart[start].filterIt(gr.ruleName(it.ruleId) == sym.nterm)
        debugecho start, " -- ", tmp, " ", sym.nterm
        tmp

  return dfSearch(ssetItems, # child,
                  isLeafp, startSet)


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

proc parseTree[C](gr: Grammar[C],
                  input: Input[C],
                  chart: Chart): Option[ParseTree[C]] =


  var triedTable: Table[TryParams, Option[ParseTree[C]]]
  proc aux(start, finish: int, name: string, level: int): Option[ParseTree[C]] =
    ## Search for parse tree of nonterminal `name`, starting at
    ## `start` and ending at `finish` position.
    let pref = &"{level:<2}|   " & "  ".repeat(level)
    template pp(body: untyped): untyped = echo pref, fmt(body)
    template pl(body: untyped): untyped =
      if false: echo pref, fmt(body)


    pl "Parsing rule {name} starting from {start}, search for alts"
    let alts: seq[int] = collect(newSeq):
      for rule in chart[start]:
        let params = TryParams(start: start, altId: rule.ruleId, name: name)
        if (params in triedTable):
          let res = triedTable[params]
          if res.isNone():
            pp "Already tried {params} and failed"
          else:
            pp "Already tried {params} and succeded"
            return res

        if (ruleName(gr, rule) == name) and (params notin triedTable):
          rule.ruleId

    if alts.len == 0:
      pp "No untried alternatives for {name} ({start}:{finish})"
    else:
      pl "There are {alts.len} rules never tried in this configuration"

    for alt in alts:
      let params = TryParams(start: start, altId: alt, name: name)
      triedTable[params] = none(ParseTree[C])
      var currpos: int = start
      var matchOk: bool = true
      pp "Trying \e[33m{name}.{alt}\e[39m [alts: {alts}] from \e[35m{currpos}\e[39m"

      block ruleTry:
        let symbols = gr.ruleBody(alt)
        let singletok = (symbols.len == 1) and (symbols[0].isTerm)
        if not singletok:
          result = some(ParseTree[C](
            isToken: false,
            ruleId: alt,
            subnodes: @[],
            start: currpos))

        for idx, sym in gr.ruleBody(alt):
          if sym.isTerm:
            pp "Expecting token #{idx} \e[93m{sym.terminal.lex}\e[39m at {currpos}"
            let inp = input currpos
            if sym.terminal.ok(inp):
              echo pref, "Matched", (if singletok: " single" else: ""),
               &" token \e[32m{sym.terminal.lex}\e[39m ",
                         &"as '\e[32m{inp.get()}\e[39m'"
              let tree = ParseTree[C](
                isToken: true,
                start: currpos,
                finish: currpos + 1,
                token: inp.get()
              )

              if singletok:
                return some(tree)
              else:
                result.get().subnodes.add tree
                inc result.get().finish

              inc currpos
              pp "@ \e[35m{currpos}\e[39m"
            else:
              pp "Failed token match \e[31m{sym.terminal.lex}\e[39m @ {currpos}"
              matchOk = false
              break ruleTry
          else:
            pp "Expecting \e[93m{sym.nterm}.{alt}\e[39m starting at {currpos}, #{idx} "
            let res = aux(currpos, finish, sym.nterm, level + 1)
            if res.isSome():
              pp "Recognized rule \e[32m{name}\e[39m in range [{currpos}, {res.get().finish}]"
              currpos = res.get().finish
              result.get().subnodes.add res.get()
              result.get().finish = currpos
              pp "@ \e[35m{currpos}\e[39m"
            else:
              pp "Failed rule \e[31m{name}\e[39m"
              matchOk = false
              break ruleTry

      if matchOk:
        triedTable[params] = result



    # for rule in chart[start]:
    #   if ruleName(gr, rule) == name:
    #     let params = (start: start, altId: rule.ruleId, name: name)
    #     if params in triedSet:
    #       pp "Already seen ({start}:{finish}), {name}, id: {rule.ruleId}"
    #       return none(ParseTree[C])
    #     else:
    #       triedSet.incl params


    #     var currpos: int = start
    #     pp "Parsing ({currpos}:{finish}), {name}, id: {rule.ruleId}"

    #     for symIdx, symbol in gr.ruleBody(rule.ruleId):
    #       pp "Extracting sym {symIdx}"
    #       if symbol.isTerm:
    #         let inp = input currpos
    #         pp "Expecting token {symbol.terminal.lex} at pos {currpos}"
    #         if symbol.terminal.ok(inp):
    #           pp "Matched {inp}"
    #           result = some(ParseTree[C](
    #             isToken: true,
    #             token: inp.get(),
    #             start: currpos,
    #             finish: currpos + 1
    #           ))

    #           inc currpos
    #       else:
        # for

    # if path.ssetItem.ruleId == -1:
    #   some(ParseTree[C](
    #     isToken: true, token: input(path.sset).get()))
    # else:
    #   let subn: seq[ParseTree[C]] = gr.topList(input, chart, path).
    #     mapIt(aux it).
    #     filterIt(it.isSome()).
    #     mapIt(it.get())

    #   some(ParseTree[C](
    #     isToken: false,
    #     ruleId: path.ssetItem.ruleId,
    #     subnodes: subn
    #   ))

  for ssetItem in chart[0]: # For all items in stateset[0]
    if ssetItem.finish == (chart.len - 1) and # If item is finished
       ruleName(gr, ssetItem) == gr.start: # And it's name is equal to
                                           # grammar start name
      let tree = aux(0, chart.len - 1, gr.start, 0) # Recognize first
                                                 # possible parse tree
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
  # printItems(grammar1, s1, onlyFull = true)
  printChart(grammar1, c1)
  let pt1    = parseTree(grammar1, input1, c1)
  echo pt1.get().lispRepr(grammar1)
  echo pt1.get().treeRepr(grammar1)
  echo "22"
