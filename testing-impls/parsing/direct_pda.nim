import std/[tables, sequtils, strformat, hashes, algorithm, strutils]
import hmisc/hasts/graphviz_ast
import hmisc/algo/clformat


type
  Pda[State, In, Stack] = object
    map: Table[(State, In, Stack), (State, seq[Stack])]
    stack: seq[Stack]
    state: State

    acceptState: State
    emptyInput: In

func dotRepr[Sa, In, St](pda: Pda[Sa, In, St]): DotGraph =
  result = makeDotGraph(dgpAutomata)
  for state in low(Sa) .. high(Sa):
    result.add makeDotNode(
      if state == pda.acceptState: dgpAutomataAccept else: dgpAutomata).
      setProps(hash(state), $state)

  for now, next in pda.map:
    result.add makeDotEdge(
      hash(now[0]), hash(next[0]),
      &"{now[1]}: {now[2]}/{hfmtIt(next[1])}"
    )

  var buf: seq[string]
  for now, next in pda.map:
    buf.add &"{now:<8} -> {next[0]}, +{hfmtIt(next[1])}"

  result.add makeRectConsolasNode().setProps(hash(buf), buf.join("\n"))
    # stdout.write &"#{idx:<2} {now:<10} -> {next:<15} :: {pda.state} {hfmtIt(pda.stack):<30}"


func newPDA[Sa, In, St](
    accept: Sa, stackStart: St, start: Sa, empty: In): Pda[Sa, In, St] =
  Pda[Sa, In, St](
    acceptState: accept,
    stack: @[stackStart],
    state: start,
    emptyInput: empty
  )

func `[]=`[Sa, In, St](
    pda: var Pda[Sa, In, St],
    state: (Sa, In, St), mapto: (Sa, seq[St])) =

  pda.map[state] = mapto

func top[Sa, In, St](pda: Pda[Sa, In, St]): St =
  pda.stack[^1]


proc run[Sa, In, St](pda: var Pda[Sa, In, St], input: seq[In]) =
  for idx, item in pairs(input):
    var now = (pda.state, item, pda.top())
    if now notin pda.map:
      now[1] = pda.emptyInput

    let next = pda.map[now]
    stdout.write &"#{idx:<2} {now:<10} -> {next:<15} ::" ,
                  &"{pda.state} {hfmtIt(pda.stack):<20}"

    pda.state = next[0]
    stdout.write &"-> {pda.stack.pop:<3}/{hfmtIt(reversed(next[1])):<7}"
    pda.stack.add reversed(next[1])

    stdout.write &"{hfmtIt(pda.stack)}\n"

  let next = pda.map[(pda.state, pda.emptyInput, pda.top())]
  pda.state = next[0]
  discard pda.stack.pop
  pda.stack.add reversed(next[1])


  echo &"End of the input, state: {pda.state}"
  if pda.state == pda.acceptState:
    echo "Accept state reached"








type
  States = enum
    stateP
    stateQ
    stateR

  Input = enum
    inZero
    inOne

    inEps

  Stack = enum
    stackA
    stackZ

func `$`(state: States): string =
  case state:
    of stateP: toStylizedAscii('P', asItalic)
    of stateQ: toStylizedAscii('Q', asItalic)
    of stateR: toStylizedAscii('R', asItalic)


func `$`(input: Input): string =
  case input:
    of inZero: "0"
    of inOne: "1"
    of inEps: "e"

func `$`(stack: Stack): string =
  case stack:
    of stackA: toStylizedAscii('A', asBold)
    of stackZ: toStylizedAscii('Z', asBold)

var pda = newPda[States, Input, Stack](stateR, stackZ, stateP, inEps)

pda[(stateP, inZero, stackZ)] = (stateP, @[stackA, stackZ])
pda[(stateP, inZero, stackA)] = (stateP, @[stackA, stackA])
pda[(stateP, inOne,  stackZ)] = (stateQ, @[])
pda[(stateP, inOne,  stackA)] = (stateQ, @[])
pda[(stateQ, inOne,  stackA)] = (stateQ, @[])
pda[(stateQ, inEps,  stackZ)] = (stateR, @[stackZ])

let graph = pda.dotRepr()
graph.toPng(AbsFile "/tmp/pda.png")

let input = concat(repeat(inZero, 5), repeat(inOne, 5))

pda.run(input)
