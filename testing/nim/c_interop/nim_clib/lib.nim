import std/[compilesettings, macros, macrocache]
import hnimast/hast_common

macro sigPrint(t: typed): untyped =
  echo t.treeRepr1()
  echo signatureHash(t)
  echo querySetting(nimcacheDir)

macro sigPrintProc(t: typed): untyped =
  echo t.treeRepr1()
  echo symBodyHash(t)

proc nonExported() = echo 123

sigPrintProc(nonExported)

nonExported()

# const exportcTable: CacheTable

type
  RefType {.exportc.} = object
    data*: string
    sub*: seq[RefType]

sigPrint(RefType)

{.emit: """/*TYPESECTION*/
typedef `RefType` NRefType;
""".}

{.push exportc.}

proc newRefType*(): RefType = RefType()

proc exportedProc(): cstring = "string from nim"

proc n_string(a: cstring): string = $a
proc n_str_int(a: int): string = $a
proc n_strlen(s: string): cint = s.len.cint
proc n_concat(a, b: string): string = a & b
proc n_concat3(a, b, c: string): string = a & b & c
proc n_cstr(a: string): cstring = cstring(a)

{.pop.}
