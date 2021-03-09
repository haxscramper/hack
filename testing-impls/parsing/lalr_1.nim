## Textbook implementation of LALR parsing algorithm. Only
## shift/reduce is implemented - goto and action tables have to be
## generated manually. Example goto/action table is identical to
## second edition of dragon book, page 252, chapter 4 "Syntax
## Analysis".

import tables
import sugar
import deques
import strformat, strutils
import sequtils

proc `[]`*(arr: seq[string], idx: string): int = arr.find(idx)

proc last*[T](arr: seq[T]): T = arr[^1]
proc first*[T](arr: seq[T]): T = arr[0]
proc first*[T](q: Deque[T]): T = q.peekFirst()

proc toQueue*[T](arr: seq[T]): Deque[T] =
  result = initDeque[T](8)
  for item in arr:
    result.addLast(item)




type
  Iterable*[T] = concept s
    s.items() is T

proc join*[T](arr: Iterable[T], sep: string = " "): string =
  var idx = 0
  for it in arr:
    if idx == 0:
      result = result & $it
    else:
      result = result & sep & $it

    inc idx

type
  Term* = object
    case isTerminal*: bool
    of true:
      val*: string
    of false:
      name*: string

  Action* = object
    ## Parse table action.
    ## :doReduce: object kind selector. Select from 'reduce' and 'shift'
    ##            action.
    ## :reduce: Number of grammar rule to apply for reduction
    ## :shift: Number of state to push onto the stack
    case doReduce*: bool
    of true:
      reduce*: int
    of false:
      shift*: int

    active*: bool ## Helper field used to distinguish between
                  ## 'regular' and 'dummy' actions
    endReached*: bool ## Special action - upon reaching parsing is
                      ## termianted.

proc `$`(t: Term): string =
  ## Convert term to string
  case t.isTerminal:
    of true: t.val
    of false: t.name

proc `$`(a: Action): string =
  ## Convert action to string
  case a.doReduce:
    of true: &"r{a.reduce}"
    of false: &"s{a.shift}"


proc tn(s: string): Term =
  ## Create new terminal
  Term(isTerminal: true, val: s)

proc nt(s: string): Term =
  ## Create new nonterminal
  Term(isTerminal: false, name: s)

func doShift(act: Action): bool =
  ## Helper function to check if action is 'shift'
  not act.doReduce

proc s(idx: int): Action =
  ## Create 'shift' action
  Action(doReduce: false, shift: idx, active: true)

proc r(idx: int): Action =
  ## Create 'reduce' action
  Action(doReduce: true, reduce: idx, active: true)

proc lalrParser(gotoTable: Table[int, seq[int]],
                actionTable: Table[int, seq[Action]],
                inputTokens: seq[string],
                gotoHeaders: seq[string],
                termHeaders: seq[string],
                grammar: Table[int, (string, seq[Term])]
               ) =

  ## Main parser function for LALR parser. It `gotoTable` and
  ## `actionTable` are part of the algorithm as described, `termHeaders`
  ## and `gotoHeaders` are implementation details. `inputTokens` - list
  ## of tokens that will be consumed by parsing algorithm
  ##   ##
  ## Parsing algorithm in pseudocode
  ##   ##
  ## .. code-block::
  ##    `a` = input.first
  ##    while true:
  ##        `s` = stack.top
  ##        if (ACTION[s, a] = shift `t`)
  ##            stack.add `t`
  ##            `a` = input.next
  ##        elif (ACTION[s, a] = reduce `A` -> `B`)
  ##            stack.pop `B.size`
  ##            `s` = stack.top
  ##            stack.add GOTO[t, A]
  ##            output A -> B
  ##        elif (ACTION[s, a] = accept)
  ##            break
  ##        else
  ##            call error
  ##   ##

  # queue of input tokens
  var input = inputTokens.toQueue()
  # stack of actions
  var stack: seq[int] = @[0]
  # current stack of symols. tokens are *shifted onto* this this list
  # from `input` when performing *shift*. When doing *reduce* some of
  # the symbols are taken from this list and replaced with
  # corresponding production.
  var symbols: seq[Term]

  var idx = 1
  while true:
    let action = actionTable[stack.last][termHeaders[input.first]]
    echo &"{\"|\":>48} action[{stack.last}, '{input.first}'] is {action}"


    let nowStr = block:
      let sym = symbols.join(" ")
      let inp = input.join(" ")
      let stk = stack.join(" ")
      &"({idx:<2}) | {stk:<8} | {sym:<10} | {inp:>15}"

    var descr: string =
      if action.doShift:
        stack.add action.shift
        symbols.add tn(input.popFirst())
        &"shift {action.shift}"
      else:
        let (nterm, reduction) = grammar[action.reduce]
        let unreduced = symbols.join(" ")
        if reduction.len == symbols.len:
          symbols = @[nt(nterm)]
        else:
          let remainingStack = symbols[0 .. reduction.len]
          symbols = remainingStack & @[nt(nterm)]

        let tmp = stack.pop()
        let goto = gotoTable[stack.last][gotoHeaders[nterm]]

        if reduction.len > 1:
          for item in 0 ..< (reduction.len - 2):
            discard stack.pop
        else:
          stack.add goto

        &"reduce by ({action.reduce}) {nterm}: [{unreduced} => {symbols.join()}] #[{stack.join}]"

    echo nowStr, " | ", descr
    if action.endReached:
      echo "reached end"
      break
    inc idx


# Grammar describes set of production rules from one nonterminal to
# several other terminals or non-terminals. Each rule is numbered.
# Each number has corresponding pair: *nonterminal* and it's
# *production*. In text it is written as `Nonterm -> P1 P2 ... PN`
# where `Pi` are terms that `Nonterm` can be replaced with. In this
# table it is encoded as `(Nonterm, @[P1, P2, ..., PN])`. For example,
# rule `E -> E + T` (first one) is encoded as `("E", @[nt "E", tn "+",
# nt "T"])`. `nt` used to create nonterminal object, `tn` is used for
# creation of terminal objects.
let grammar ={
  1 : ("E", @[nt "E", tn "+", nt "T"]),
  2 : ("E", @[nt "T"]),
  3 : ("T", @[nt "T", tn "*", nt "F"]),
  4 : ("T", @[nt "F"]),
  5 : ("F", @[tn "(", nt "E", tn ")"]),
  6 : ("F", @[tn "id"])
}.toTable()



# This is helper variable that denotes empty action. There are not
# sparse sequences in nim so I had to plug dummy variable. It serves
# no real purpose other than occupy space in table.
let n = Action(active: false)

# Special action that denotes end of parsing.
let acc = Action(endReached: true)

# This sort of awkward formatting was chosen to imitate parsing table
# for expression grammar (figure 4.37, p252). `*Header` variables are
# headers of the table (GOTO and ACTION), table keys correspond to
# STATE.

# GOTO table is mapping `<state-number> X <term> -> <state-number>`.
# When reduction is performed it is used for getting number of next
# state: old state numbers are removed from stack and replaced with
# GOTO[s, A] (p252, second paragraph '... state symbols is popped off
# the stack. State 0 is then exposed. Since goto of state 0 on F is 3,
# state 3 is pushed onto the stack ...').
let gotoHeaders =                         @["E", "T", "F"]

# ACTION table is mapping `<state-number> X <token> -> <action>`.
# After parser performs an action (shift or reduction) this table used
# to look up next action to perform. Current action number is held on
# stack - top value is used to select ACTION[s, t] - `s` is an action
# from stack top, `t` is toke in input. NOTE: token is not moved from
# input queue unless action is 'shift'
let termHeaders =
        @["id", "+", "*", "(", ")",  "$"]
let parsingTable = {
  0  : (@[s 5,  n,   n,   s 4, n,    n],   @[1,  2,  3]),
  1  : (@[n,    s 6, n,   n,   n,    acc], @[-1, -1, -1]),
  2  : (@[n,    r 2, s 7, n,   r 2,  r 2], @[-1, -1, -1]),
  3  : (@[n,    r 4, r 4, n,   r 4,  r 4], @[-1, -1, -1]),
  4  : (@[s 5,  n,   n,   s 4, n,    n],   @[8,  2,  3]),
  5  : (@[n,    r 6, r 6, n,   r 6,  r 6], @[-1, -1, -1]),
  6  : (@[s 5,  n,   n,   s 4, n,    n],   @[-1, 9,  3]),
  7  : (@[s 5,  n,   n,   s 4, n,    n],   @[-1, -1, 10]),
  8  : (@[n,    s 6, n,   n,   s 11, n],   @[-1, -1, -1]),
  9  : (@[n,    r 1, s 7, n,   r  1, r 1], @[-1, -1, -1]),
  10 : (@[n,    r 3, r 3, n,   r 3,  r 3], @[-1, -1, -1]),
  11 : (@[n,    r 5, r 5, n,   s 5,  r 5], @[-1, -1, -1]),
}.toTable()

# Separating parse table into action table and goto table. This is
# necessary only because parse table was specified as a single literal
# value. In real application these tables are generated separately
let actionTable = collect(initTable):
  for key, val in parsingTable:
    {key : val[0]}

let gotoTable = collect(initTable):
  for key, val in parsingTable:
    {key : val[1]}


lalrParser(
  gotoTable = gotoTable,
  actionTable = actionTable,
  inputTokens = @["id", "*", "id", "+", "id", "$"],
  termHeaders = termHeaders,
  gotoHeaders = gotoHeaders,
  grammar = grammar
)
