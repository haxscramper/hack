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

proc `[]`(arr: seq[string], idx: string): int = arr.find(idx)

proc last[T](arr: seq[T]): T = arr[^1]
proc first[T](arr: seq[T]): T = arr[0]
proc first[T](q: Deque[T]): T = q.peekFirst()

proc toQueue[T](arr: seq[T]): Deque[T] =
  result = initDeque[T](8)
  for item in arr:
    result.addLast(item)




type
  Iterable[T] = concept s
    s.items() is T

proc join[T](arr: Iterable[T], sep: string = " "): string =
  var idx = 0
  for it in arr:
    if idx == 0:
      result = result & $it
    else:
      result = result & sep & $it

    inc idx

type
  Term = object
    case isTerminal: bool
    of true:
      val: string
    of false:
      name: string

  Action = object
    case doReduce: bool
    of true:
      reduce: int
    of false:
      shift: int

    active: bool
    endReached: bool

proc `$`(t: Term): string =
  case t.isTerminal:
    of true: t.val
    of false: t.name

proc `$`(a: Action): string =
  case a.doReduce:
    of true: &"r{a.reduce}"
    of false: &"s{a.shift}"


proc tn(s: string): Term = Term(isTerminal: true, val: s)
proc nt(s: string): Term = Term(isTerminal: false, name: s)
func doShift(act: Action): bool = not act.doReduce
proc s(idx: int): Action = Action(doReduce: false, shift: idx, active: true)
proc r(idx: int): Action = Action(doReduce: true, reduce: idx, active: true)

proc lalrParser(gotoTable: Table[int, seq[int]],
                actionTable: Table[int, seq[Action]],
                inputTokens: seq[string],
                gotoHeaders: seq[string],
                termHeaders: seq[string],
                grammar: Table[int, (string, seq[Term])]
               ) =
  var input = inputTokens.toQueue()
  var stack: seq[int] = @[0]
  var symbols: seq[Term]

  var idx = 1
  while idx < 16:
    stdout.write &"{\"|\":>48} action[{stack.last}, '{input.first}'] "
    let action = actionTable[stack.last][termHeaders[input.first]]
    stdout.writeLine &"is {action}"


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


let grammar ={
  1 : ("E", @[nt "E", tn "+", nt "T"]),
  2 : ("E", @[nt "T"]),
  3 : ("T", @[nt "T", tn "*", nt "F"]),
  4 : ("T", @[nt "F"]),
  5 : ("F", @[tn "(", nt "E", tn ")"]),
  6 : ("F", @[tn "id"])
}.toTable()



let n = Action(active: false)
let acc = Action(endReached: true)

let gotoHeaders =                         @["E", "T", "F"]
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
