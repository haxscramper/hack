import
  hnimast

import
  hmisc/core/[all, code_errors, colored],
  hmisc/types/colorstring,
  hmisc/algo/clformat

import
  std/[sequtils, tables, strformat, sets, strutils, stats, math]

import
  hmisc/other/oswrap

type
  PackageStat = object
    name: string
    readmeLen: int
    hasLicense: bool
    testCount: seq[int]
    totalEntries: int
    totalSloc: int
    documentedEntries: int

var list: seq[PackageStat]

proc registerDir(dir: AbsDir) =
  if globalTick() > 10_000:
    return

  var pack = PackageStat(name: dir.name())

  for file in walkDir(dir, RelFile, recurse = true, exts = @["nim"]):
    if "tests" in file:
      pack.testCount.add readFile(dir / file).count('\n')

    else:
      let text = readFile(dir / file)
      pack.totalSloc += text.count('\n')
      let node = parsePNodeStr(text, doRaise = false)
      # if notNil(node):
      #   try:
      #     register(node, file, pack)

        # except Exception as e:
        #   pprintStackTrace(e)

  for file in walkDir(dir, RelFile):
    if "readme" in normalize($file):
      pack.readmeLen = readFile(dir / file).count('\n')

    if "license" in normalize($file):
      pack.hasLicense = true




  list.add pack


for dir in walkDir(cwd() / "main/packages", AbsDir):
  if dir.name() notin @["bluu", "netwatch", "gcplat", "atoz", "compiler", "finalseg"]:
    echo dir.name()
    registerDir(dir)

echo &"""
avg readme: {list.mapIt(it.readmeLen.float).mean():5.3f}
no readme:  {len(list.filterIt(it.readmeLen == 0))}
avg tests:  {list.mapIt(it.testCount.sum().float).mean():5.3f}
avg sloc:   {list.mapIt(it.totalSloc.float).mean():5.3f}
total pack: {list.len}
"""
