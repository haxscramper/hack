import std/[random, sequtils]
import pkg/[benchy]

type
  OpcCxx {.importcpp: "Opc", inheritable, pure, header: "cxx_opcodes.hpp".} = object
  OpcInc {.importcpp: "OpcInc".} = object of OpcCxx
  OpcDec {.importcpp: "OpcDec".} = object of OpcCxx
  OpcMul2 {.importcpp: "OpcMul2".} = object of OpcCxx
  OpcDiv2 {.importcpp: "OpcDiv2".} = object of OpcCxx
  OpcAdd7 {.importcpp: "OpcAdd7".} = object of OpcCxx
  OpcNeg {.importcpp: "OpcNeg".} = object of OpcCxx

proc newOpcCxx(): ptr OpcCxx {.importcpp: "new Opc()".}
proc newOpcInc(): ptr OpcInc {.importcpp: "new OpcInc()".}
proc newOpcDec(): ptr OpcDec {.importcpp: "new OpcDec()".}

proc newOpcMul2(): ptr OpcMul2 {.importcpp: "new OpcMul2()".}
proc newOpcDiv2(): ptr OpcDiv2 {.importcpp: "new OpcDiv2()".}
proc newOpcAdd7(): ptr OpcAdd7 {.importcpp: "new OpcAdd7()".}
proc newOpcNeg(): ptr OpcNeg {.importcpp: "new OpcNeg()".}


proc eval(op: OpcCxx, val: ptr cint) {.importcpp: "#.eval(@)".}


type
  OpcNimKind = enum
    Inc  # = 0x0100
    Dec  # = 0x0110
    Mul2 # = 0x0230
    Div2 # = 0x0240
    Add7 # = 0x0307
    Neg  # = 0x0400

  OpcNim = object
    kind: OpcNimKind

proc newOpcNim(kind: OpcNimKind): ref OpcNim = (ref OpcNim)(kind: kind)
proc initOpcNim(kind: OpcNimKind): OpcNim = OpcNim(kind: kind)


proc eval(opc: OpcNim, result: ptr cint) =
  case opc.kind:
    of Inc:  inc result[]
    of Dec:  dec result[]
    of Mul2: result[] *= 2
    of Div2: result[] = result[] div 2
    of Add7: inc result[], 7
    of Neg:  result[] = -result[]

type
  EvalProcType = proc(result: ptr cint) {.cdecl.}
  OpcVtable = array[0 .. 0, pointer]
  OpcVt = object
    vtable: ref OpcVtable

proc eval(opc: OpcVt, result: ptr cint) =
  cast[EvalProcType](opc.vtable[][0])(result)

proc newOpcVt(impl: EvalProcType): OpcVt =
  new(result.vtable)
  result.vtable[][0] = cast[pointer](impl)

proc main()=
  var r = initRand(42)

  var Nb_Instructions = 1000_000

  var result: cint = 0
  var aRes = addr result


  block:
    var opsVt: seq[OpcVt] = @[
      newOpcVt(proc(result: ptr cint) {.cdecl.} = inc result[]),
      newOpcVt(proc(result: ptr cint) {.cdecl.} = dec result[]),
      newOpcVt(proc(result: ptr cint) {.cdecl.} = result[] *= 2),
      newOpcVt(proc(result: ptr cint) {.cdecl.} = result[] = result[] div 2),
      newOpcVt(proc(result: ptr cint) {.cdecl.} = inc result[], 7),
      newOpcVt(proc(result: ptr cint) {.cdecl.} = result[] = -result[]),
    ]

    let instructionsVt = newSeqWith(Nb_Instructions, r.sample(opsVt))
    timeIt "Nim manual vtable":
      for instr in instructionsVt:
        eval(instr, aRes)


  block:
    var opsCxx: seq[ptr OpcCxx] = @[
      cast[ptr OpcCxx](newOpcInc()),
      newOpcDec(),
      newOpcMul2(),
      newOpcDiv2(),
      newOpcAdd7(),
      newOpcNeg()
    ]

    let instructionsCxx = newSeqWith(Nb_Instructions, r.sample(opsCxx))
    timeIt "Cxx ops":
      for instr in instructionsCxx:
        eval(instr[], aRes)

  block:
    var opsNim: seq[ref OpcNim] = @[
      newOpcNim(Inc),
      newOpcNim(Dec),
      newOpcNim(Mul2),
      newOpcNim(Div2),
      newOpcNim(Add7),
      newOpcNim(Neg)
    ]


    let instructionsNim = newSeqWith(Nb_Instructions, r.sample(opsNim))
    timeIt "Nim ops":
      for instr in instructionsNim:
        eval(instr[], aRes)

  block:
    var opsNimVal: seq[OpcNim] = @[
      initOpcNim(Inc),
      initOpcNim(Dec),
      initOpcNim(Mul2),
      initOpcNim(Div2),
      initOpcNim(Add7),
      initOpcNim(Neg)
    ]



    let instructionsNimVal = newSeqWith(Nb_Instructions, r.sample(opsNimVal))
    timeIt "Nim ops val":
      for instr in instructionsNimVal:
        eval(instr, aRes)



main()
