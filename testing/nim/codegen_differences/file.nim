import std/[macros, strutils, sequtils, strformat]

func restack(a: string): string =
  a.strip().split("\n").mapIt("// " & it).join("\n")

template cblock(a: untyped): untyped =

  block:
    const iinfo {.inject.} = instantiationInfo()
    const blockId = &"/* BLOCK-START {iinfo.line} */"
    {.emit: blockId.}
    const text = restack(astToStr(a))
    {.emit: text.}
    {.emit: "/* BLOCK-END-NIM */"}
    block:
      a

    {.emit: "/* BLOCK-END */".}

proc main() =
  {.emit: "/* SECTION-BEFORE */"}

  cblock:
    var v = 123
    var p = addr v
    var adr = addr p[]
    echo 1

  {.emit: "/* SECTION-AFTER */"}

main()
