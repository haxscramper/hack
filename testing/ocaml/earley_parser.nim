import sugar, strutils, sequtils, strformat, options, sets

func `|>`[A, B](a: A, b: A -> B): B = b(a)

func `|-`[A, B, C](f: A -> B, g: B -> C, x: A): C = g (f x)

func id[A](x: A): A = x

func swap[A, B, C](f: (A, B) -> C, b: B, a: A): C = f(a, b)

func `>>=`[A, B](a: Option[A], f: A -> B): Option[B] =
  if a.isSome(): some f(a.get()) else: none(B)

func `//`[A](ins: seq[A], pr: A -> bool): seq[A] = ins.filter(pr)
func `/@`[A, B](ins: seq[A], f: A -> B): seq[B] = ins.map(f)

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
    rhs: seq[Rule[C]]

  EItem[C] = object
    ruleId: int
    startPos: int
    nextPos: int

  Chart[C] = seq[seq[EItem[C]]]

  Grammar[C] = object
    rules: seq[Rule[C]]

func contains(s: string, ns: NullSet): bool = s in ns.nulls

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
  gr[idx].rhs

func nextSymbol[C](gr: Grammar[C], item: EItem[C]): Option[Symbol[C]] =
  #[ IMPLEMENT ]#
  discard

func append[A](a: var seq[A], b: A): int =
  a.add b
  return a.len

func buildItems[C](gr: Grammar[C],
                   input: int -> Option[C]): Chart[C] =

  func predict(s: Chart[C], # DOC
               currPos: int,
               j: int, # DOC
               nullable: NullSet,
               symbol: string # FIXME ???
              ): int =
    discard

  func scan(s: Chart[C], i, j: int, symbol: Tok[C]): int = discard
  func complete(s: Char[C], i, j: int): int = discard


  let nullable = nullableSymbols gr
  var s: Chart[C] = @[@[]]

  # TODO (* Seed s with the start symbol *)

  var i = 0 # DOC
  while i < s.len:
    var j = 0 # DOC
    while j < s[i].len:
      let next: Option[Symbol[C]] = gr.nextSymbol(s[i][j])
      if next.isNone():
        complete(s, i, j)
      else:
        let sym: Symbol[C] = next.get()
        if sym.isTerm:
          predict(s, i, j, nullable, sym.name)
        else:
          scan(s, i, j, sym.terminal.ok)


  discard
