## Wrapper for ngspice library

import sequtils
import math, complex
import sugar

{.passl: "-lngspice" .}

const hdr = "sharedspice.h"

type
  CArray[T] = UncheckedArray[T]

type
  NGVecInfo_Impl {.header(hdr), importc: "vecinfoall".} = object
    name: cstring
    title: cstring
    date: cstring
    `type`: cstring
    veccount: cint

  NGVecInfo = object
    name: string
    title: string
    date: string
    veccount: int
    vtype: string

converter toNGVecInfo(impl: NGVecInfo_Impl): NGVecInfo =
  NGVecInfo(
    name: $impl.name,
    title: $impl.title,
    veccount: impl.veccount,
    vtype: $impl.`type`,
    date: $impl.date
  )

type
  NGComplex {.header(hdr), importc: "ngcomplex".} = object
    cx_real: cdouble
    cx_imag: cdouble

  NGVectorInfo_Impl {.header(hdr), importc: "vector_info".} = object
    v_name: cstring ## Same as so_vname.
    v_type: cint ## Same as so_vtype.
    v_flags: cshort ## Flags (a combination of VF_*).
    v_realdata: CArray[cdouble] ## Real data.
    v_compdata: CArray[NGComplex] ## Complex data.
    v_length: cint ## Length of the vector.

  NGVectorInfo = object
    name: string
    vtype: int
    flags: int
    realdata: seq[float]
    compdata: seq[Complex[float]]
    length: int

converter toComplex(val: NGComplex): Complex[float] =
  Complex[float](
    re: val.cx_real,
    im: val.cx_imag
  )

converter toNGVectorInfo(impl: NGVectorInfo_Impl): NGVectorInfo =
  NGVectorInfo(
    name: $impl.v_name,
    vtype: impl.v_type,
    flags: impl.v_flags,
    realdata: (
      block:
        collect(newSeq):
          for i in 0 ..< impl.v_length: impl.v_realdata[i])
  )


type
  NGSendDataCb = proc(
    a1: ptr NGVecInfo_Impl, a2: cint, a3: cint, a4: pointer): cint {.cdecl.}

  NGSendCharCb = proc(a1: cstring, a2: cint, a3: pointer): cint {.cdecl.}

  NGSendStatCb = proc(a1: cstring, a2: cint, a3: pointer): cint {.cdecl.}

  NGControlledExitCb = proc(
    a1: cint, a2: bool, a3: bool, a4: int, a5: pointer): cint {.cdecl.}

  NGSendInitDataCb = proc(
    a1: ptr NGVecInfo_Impl, a2: cint, a3: pointer): cint {.cdecl.}

  NGBGThreadRunningCb = proc(a1: bool, a2: cint, a3: pointer): cint {.cdecl.}

proc ngPrintPassthrough(a1: cstring, a2: cint, a3: pointer): cint {.cdecl.} =
  echo a1
  return 0

proc ngDefaultExit(
  a1: cint, a2: bool, a3: bool, a4: int, a5: pointer): cint {.cdecl.} =
    discard

proc ngNoMultithreading(a1: bool, a2: cint, a3: pointer): cint {.cdecl.} =
  return 0

var currentVeccount: cint = 0

proc ngDefaultSendInitData(
  a1: ptr NGVecInfo_Impl, a2: cint, a3: pointer): cint {.cdecl.} =
  currentVeccount = a1.veccount


proc ngDefaultSendData(
  a1: ptr NGVecInfo_Impl, a2: cint, a3: cint, a4: pointer): cint {.cdecl.} =
    discard

proc ngspiceInit_Impl(
  printfcn: NGSendCharCb = ngPrintPassthrough,
  statfcn: NGSendStatCb = ngPrintPassthrough,
  ngexit: NGControlledExitCb = ngDefaultExit,
  sdata: NGSendDataCb = ngDefaultSendData,
  sinitdata: NGSendInitDataCb = ngDefaultSendInitData,
  bgtrun: NGBGThreadRunningCb = ngNoMultithreading,
  userData: pointer = nil
                     ) {.importc("ngSpice_Init"), header(hdr).}

proc ngSpiceCommand_Impl(
  a1: cstring): cint {.importc("ngSpice_Command"), header(hdr).}

proc ngspiceCommand(arg: string): int {.discardable.} =
  ## Run ngspice command
  var str = allocCStringArray([arg])
  result = ngSpiceCommand_Impl(a1 = str[0])
  deallocCStringArray(str)

proc ngSpice_Circ_Impl(circarray: cstringArray): cint
  {.importc("ngSpice_Circ"), header(hdr).}

proc ngSpiceCirc(circarray: seq[string],
  header: string = "Circuit simulation", footer: string = ".end"
                ): int {.discardable.} =

  # arr[circarray.len + 3] = cast[cstring](0)
  let input = @[header] & circarray & @[footer] & @[""]
  let arr = allocCStringArray(input)
  arr[input.len - 1] = cast[cstring](0)
  result = ngSpice_Circ_Impl(arr)
  # deallocCStringArray(arr)

proc ngSpiceCurPlot_Impl(): cstring {.importc("ngSpice_CurPlot"), header(hdr).}
proc ngSpiceCurPlot(): string =
  ## Get name of the last simulated plot
  $ngSpiceCurPlot_Impl()

proc ngSpiceAllVecs_Impl(pltname: cstring): cstringArray
  {.importc("ngSpice_AllVecs"), header(hdr).}

proc ngSpiceAllVecs(pltname: string): seq[string] =
  ## Get vector names associated with plot
  let cpltname = allocCStringArray([pltname])
  let tmp = ngSpiceAllVecs_Impl(cpltname[0])

  for i in 0 ..< currentVeccount:
    result.add $tmp[i]

  deallocCStringArray(cpltname)

proc ngGet_Vec_Info_Impl(plotvecname: cstring): ptr NGVectorInfo_Impl
  {.importc("ngGet_Vec_Info"), header(hdr).}

proc ngGetVecInfo(plotvecname: string): NGVectorInfo =
  let cplotvecname = allocCStringArray([plotvecname])
  let impl = ngGet_Vec_Info_Impl(cplotvecname[0])
  result = impl[]
  deallocCStringArray(cplotvecname)

when isMainModule:
  ngspiceInit_Impl()

  ngSpiceCirc(
    @[
      "V1 0 1 5",
      "V2 0 2 5",
      "R1 0 1 10",
      "R2 0 2 10",
      ".dc v1 0 5 1"
    ]
  )

  ngSpice_Command("run");

  let cp = ngSpiceCurPlot()
  for vec in ngSpiceAllVecs(cp):
    let res = ngGetVecInfo(cp & "." & vec)
    echo vec, ":  ", res.realdata

  echo "done"
