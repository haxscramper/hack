import std/[random, sequtils, times, strformat, algorithm]
import pkg/[benchy]

{.push overflowChecks:off.}

type
  Op = enum
    Halt # = 0x0000
    Inc  # = 0x0100
    Dec  # = 0x0110
    Mul2 # = 0x0230
    Div2 # = 0x0240
    Add7 # = 0x0307
    Neg  # = 0x0400

template switch(op: typed): untyped =
  case op:
    of Halt: return
    of Inc:  inc pc; inc result
    of Dec:  inc pc; dec result
    of Mul2: inc pc; result *= 2
    of Div2: inc pc; result = result div 2
    of Add7: inc pc; inc result, 7
    of Neg:  inc pc; result = -result


func interp_switch(code: seq[Op], initVal: int): int =
  var pc = 0
  result = initVal

  while true:
    switch(code[pc])

func interp_cgoto_assign(code: seq[Op], initVal: int): int =
  var pc = 0
  result = initVal

  while true:
    {.computedGoto.}
    let instr = code[pc]
    switch(instr)

func interp_cgoto(code: seq[Op], initVal: int): int =
  var pc = 0
  result = initVal

  while true:
    {.computedGoto.}
    switch(code[pc])

func halt(result: var int, stop: var bool) {.inline, nimcall.}=
  stop = true

func inc(result: var int, stop: var bool) {.inline, nimcall.}=
  inc result

func dec(result: var int, stop: var bool) {.inline, nimcall.}=
  dec result

func mul2(result: var int, stop: var bool) {.inline, nimcall.}=
  result *= 2

func div2(result: var int, stop: var bool) {.inline, nimcall.}=
  result = result div 2

func add7(result: var int, stop: var bool) {.inline, nimcall.}=
  inc result, 7

func neg(result: var int, stop: var bool) {.inline, nimcall.}=
  result = -result

# Requires dense enum
type InstrF = proc (result: var int, stop: var bool){.inline, nimcall, noSideEffect, gcsafe, locks: 0.}

type FuncTable = array[Op, InstrF]

const funcTable: FuncTable = [
  Halt: halt,
  Inc: inc,
  Dec: dec,
  Mul2: mul2,
  Div2: div2,
  Add7: add7,
  Neg: neg
]

proc interp_ftable(code: seq[Op], initVal: int): int =
  # Requires dense enum
  var
    pc = 0
    stop = false
  result = initVal

  while not stop:
    funcTable[code[pc]](result, stop)
    inc pc

#################################################################################################################

type
  InstrNext = proc (val: var int, code: seq[Op], pc: var int, stop: var bool): OpH {.inline, nimcall.}

  OpH = ref object
    handler: InstrNext

  FuncTableNext = array[Op, OpH]

proc halt(val: var int, code: seq[Op], pc: var int, stop: var bool): OpH {.inline, nimcall.}
proc inc(val: var int, code: seq[Op], pc: var int, stop: var bool): OpH {.inline, nimcall.}
proc dec(val: var int, code: seq[Op], pc: var int, stop: var bool): OpH {.inline, nimcall.}
proc mul2(val: var int, code: seq[Op], pc: var int, stop: var bool): OpH {.inline, nimcall.}
proc div2(val: var int, code: seq[Op], pc: var int, stop: var bool): OpH {.inline, nimcall.}
proc add7(val: var int, code: seq[Op], pc: var int, stop: var bool): OpH {.inline, nimcall.}
proc neg(val: var int, code: seq[Op], pc: var int, stop: var bool): OpH {.inline, nimcall.}

let funcTableNext: FuncTableNext = [
  Halt: OpH(handler: halt),
  Inc: OpH(handler: inc),
  Dec: OpH(handler: dec),
  Mul2: OpH(handler: mul2),
  Div2: OpH(handler: div2),
  Add7: OpH(handler: add7),
  Neg: OpH(handler: neg)
]

proc halt(val: var int, code: seq[Op], pc: var int, stop: var bool): OpH {.inline, nimcall.}=
  stop = true

proc inc(val: var int, code: seq[Op], pc: var int, stop: var bool): OpH {.inline, nimcall.}=

  inc val
  inc pc
  result = funcTableNext[code[pc]]

proc dec(val: var int, code: seq[Op], pc: var int, stop: var bool): OpH {.inline, nimcall.}=
  dec val
  inc pc
  result = funcTableNext[code[pc]]

proc mul2(val: var int, code: seq[Op], pc: var int, stop: var bool): OpH {.inline, nimcall.}=
  val *= 2
  inc pc
  result = funcTableNext[code[pc]]

proc div2(val: var int, code: seq[Op], pc: var int, stop: var bool): OpH {.inline, nimcall.}=
  val = val div 2
  inc pc
  result = funcTableNext[code[pc]]

proc add7(val: var int, code: seq[Op], pc: var int, stop: var bool): OpH {.inline, nimcall.}=
  inc val, 7
  inc pc
  result = funcTableNext[code[pc]]

proc neg(val: var int, code: seq[Op], pc: var int, stop: var bool): OpH {.inline, nimcall.}=
  val = -val
  inc pc
  result = funcTableNext[code[pc]]

proc interp_handlers(code: seq[Op], initVal: int): int =
  # Requires dense enum
  var
    pc = 0
    stop = false
    oph = funcTableNext[code[pc]]
  result = initVal

  while not stop:
    oph = oph.handler(result, code, pc, stop)

{.pop.}

#################################################################################################################

import random, sequtils, times, os, strutils, strformat

const Nb_Instructions = 10_000_000

type Res = tuple[name: string, elapsed, mips: float]

template bench(impl: untyped): Res =
  var res: Res; res.name = astToStr(impl)
  block:
    let
      start = cpuTime()
      r = impl(instructions, n)
      stop = cpuTIme()
      elapsed {.inject.} = stop - start
      mips {.inject.} = (Nb_Instructions.float / (1_000_000.0 * elapsed))

    res.elapsed = elapsed
    res.mips = mips

  res


type
  OpD = ref object {.inheritable.}

  Ohalt {.final.}= ref object of OpD
  Oinc {.final.}= ref object of OpD
  Odec {.final.}= ref object of OpD
  Omul2 {.final.}= ref object of OpD
  Odiv2 {.final.}= ref object of OpD
  Oadd7 {.final.}= ref object of OpD
  Oneg {.final.}= ref object of OpD

  FuncTableToken = array[Op, OpD]

method execute(op: OpD, result: var int, stop: var bool) {.base, inline, noSideEffect.} =
  raise newException(ValueError, "To override")

method execute(op: Ohalt, result: var int, stop: var bool) {.inline, noSideEffect.}=
  stop = true

method execute(op: Oinc, result: var int, stop: var bool) {.inline, noSideEffect.}=
  inc result

method execute(op: Odec, result: var int, stop: var bool) {.inline, noSideEffect.}=
  dec result

method execute(op: Omul2, result: var int, stop: var bool) {.inline, noSideEffect.}=
  result *= 2

method execute(op: Odiv2, result: var int, stop: var bool) {.inline, noSideEffect.}=
  result = result div 2

method execute(op: Oadd7, result: var int, stop: var bool) {.inline, noSideEffect.}=
  inc result, 7

method execute(op: Oneg, result: var int, stop: var bool) {.inline, noSideEffect.}=
  result = -result

let funcTableToken: FuncTableToken = [
  Halt: Ohalt(),
  Inc: Oinc(),
  Dec: Odec(),
  Mul2: Omul2(),
  Div2: Odiv2(),
  Add7: Oadd7(),
  Neg: Oneg()
]

proc interp_methods(code: seq[Op], initVal: int): int =
  # Requires dense enum
  var
    pc = 0
    stop = false
    opt: OpD
  result = initVal

  while not stop:
    opt = funcTableToken[code[pc]]
    opt.execute(result, stop)
    inc pc

proc main(n: int, benchy: bool = false) =
  var r = initRand(42)

  let ops = [Inc, Dec, Div2, Add7, Neg]
  let instructions = newSeqWith(Nb_Instructions, r.sample(ops)) & Halt

  if benchy:
    timeIt "switch":
      discard bench(interp_switch)

    timeIt "cgoto":
      discard bench(interp_cgoto)

    timeIt "cgoto assign":
      discard bench(interp_cgoto_assign)

    timeIt "ftable":
      discard bench(interp_ftable)

    timeIt "handlers":
      discard bench(interp_handlers)

    timeIt "methods":
      discard bench(interp_methods)


  else:
    var stats: seq[Res]
    stats.add bench(interp_switch)
    stats.add bench(interp_cgoto)
    stats.add bench(interp_cgoto_assign)
    stats.add bench(interp_ftable)
    stats.add bench(interp_handlers)
    stats.add bench(interp_methods)

    for (name, elapsed, mips) in stats.sortedByIt(it.mips):
      echo &"{name:<20} took {elapsed:<1.4}. Mips: {mips:<3.5}"


var start = cpuTime()
block:
  var foo = 123
  for i in 0 ..< 1_000_000:
    foo += i*i mod 456
    foo = foo mod 789

# Compiler shouldn't optimize away the results as cpuTime rely on sideeffects
var stop = cpuTime()
echo "Warmup: " & $(stop - start) & "s"

main(1, false)
main(1, true)
