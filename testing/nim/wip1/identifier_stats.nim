import times, stats, strformat, os, strutils

import compiler/[parser, llstream, idents, options,
                 pathutils, astalgo, ast, renderer]

import tables


var filecount = 0
var unparsable: seq[string]
var sloc = 0
proc parsefile(file: string): PNode =
  var pars: TParser
  let cache: IdentCache = newIdentCache()
  let config: ConfigRef = newConfigRef()

  let strs = file.readFile()
  openParser(
    p = pars,
    filename = AbsoluteFile(file),
    inputStream = llStreamOpen(strs),
    cache = cache,
    config = config
  )

  try:
    result = parseAll(pars)
    sloc += strs.split("\n").len()
  except:
    unparsable.add file
    echo "file ", file, " could not be parsed"

  closeParser(pars)

func isSnakeCase*(str: string): bool =
  result = true
  for c in str:
    if c notin {'a' .. 'z', 'A' .. 'Z', '_', '0' .. '9'}:
      return false

func allLowercase*(str: string): bool =
  result = true
  for c in str:
    if c notin {'a' .. 'z', '0' .. '9'}:
      return false


func isCamelCase*(str: string): bool =
  result = true
  if allLowercase(str):
    return true

  for c in str:
    if c notin {'a' .. 'z', 'A' .. 'Z', '0' .. '9'}:
      return false

var
  snakeCount: int = 0
  camelCount: int = 0
  wtfcase: int = 0

func idstr*(node: PNode): string = node.ident.s

proc walktree(node: PNode): void =
  case node.kind:
    of nkIdent:
      let str = node.idstr
      if str.isCamelCase():
        inc camelCount
      elif str.isSnakeCase():
        inc snakeCount
      else:
        inc wtfcase
    else:
      discard

  for child in node:
    walktree(child)


if false: # generate identifier stats
  for file in walkDirRec("/tmp/nimrepos"):
    if file.endsWith(".nim"):
      let node = file.parsefile()
      if node != nil:
        inc filecount
        node.walktree()

  echo &"Found {snakeCount} snakes and {camelCount} camels"
  echo "Analyzed files in total ", filecount
  echo "Source lines count: ", sloc
  echo unparsable.len, " files could not be parsed"
  echo "wtfcase: ", wtfcase

proc getFirstRes*(node: PNode): PNode =
  for subn in node:
    for elem in subn:
      if (elem.kind == nkIdent) and (elem.ident.s == "result"):
        return subn

    result = subn.getFirstRes()
    if result != nil:
      return result

var cnt: CountTable[string]
if true:
  for file in walkDirRec("/mnt/defaultdirs/nimrepos/"):
    if file.endsWith(".nim"):
      echo file
      let node = file.parseFile()
      if node != nil:
        let firstRes = node.getFirstRes()
        if firstRes != nil:
          cnt.inc($firstRes.kind)

  echo cnt

"unparsable.txt".writeFile(unparsable.join("\n"))
