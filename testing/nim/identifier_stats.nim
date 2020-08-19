import times, stats, strformat, os, strutils

import compiler/[parser, llstream, idents, options, pathutils, astalgo, ast]

var pars: TParser
let cache: IdentCache = newIdentCache()
let config: ConfigRef = newConfigRef()

proc parsefile(file: string): PNode =
  openParser(
    p = pars,
    filename = AbsoluteFile(file),
    inputStream = llStreamOpen(file.readFile()),
    cache = cache,
    config = config
  )

  result = parseAll(pars)
  closeParser(pars)

func isSnakeCase*(str: string): bool =
  result = true
  for c in str:
    if c notin {'a' .. 'z', '_', '0' .. '9'}:
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

func idstr*(node: PNode): string = node.ident.s

proc walktree(node: PNode): void =
  case node.kind:
    of nkIdent:
      let str = node.idstr
      if str.isCamelCase():
        inc camelCount
      elif str.isSnakeCase():
        # echo str
        inc snakeCount
    else:
      discard

  for child in node:
    walktree(child)

for file in walkDirRec("/tmp/nimrepos"):
  if file.endsWith(".nim"):
    # echo file
    file.parsefile().walktree()

echo &"Found {snakeCount} snakes and {camelCount} camels"
