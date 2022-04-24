import std/[strutils, re, algorithm, tables, sequtils, strformat]

let cfile = "cache_c/@mfile.nim.c"
let cppfile = "cache_cpp/@mfile.nim.cpp"

type
  CodeBlock = object
    id: int
    ccode: seq[string]
    cppcode: seq[string]
    original: seq[string]

var inSection = true
var blocks: Table[int, CodeBlock]
var currentBlock = 0
var inNimCount = 0

for (file, cpp) in {cfile: false, cppfile: true}:
  for line in lines(file):
    if "SECTION-BEFORE" in line:
      inSection = true

    elif "SECTION-AFTER" in line:
      inSection = false

    elif inSection:
      if line =~ re".*BLOCK-START (\d+).*":
        echo "found block start ", matches[0]
        let bl = CodeBlock(id: parseInt(matches[0]))
        if bl.id notin blocks:
          blocks[bl.id] = bl
          # only trigger nim code collection once, if block ID is new
          inNimCount = 1

        else:
          inNimCount = 2

        currentBlock = bl.id

      elif "BLOCK-END-NIM" in line:
        inNimCount = 0

      elif "BLOCK-END" in line:
        currentBlock = 0

      else:
        if currentBlock == 0 or inNimCount == 2:
          discard

        elif inNimCount == 1:
          blocks[currentBlock].original.add line

        elif cpp:
          # echo "C  ", file, line
          blocks[currentBlock].cppCode.add line

        else:
          # echo "CXX", file, line
          blocks[currentBlock].ccode.add line

var res: string

for bl in blocks.values().toSeq().sortedByIt(it.id):
  let orig = bl.original.
    join("\n").
    dedent().
    split("\n").
    mapIt(it[3..^1]).
    join("\n").
    dedent()

  let c = bl.ccode[1..^2].join("\n").dedent()
  let cpp = bl.cppcode[1..^2].join("\n").dedent()
  res.add &"""
```nim
{orig}
```

```c
{c}
```

```c++
{cpp}
```

"""

writeFile("results.md", res)
echo res
