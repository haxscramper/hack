import nimsuggest/sexp, std/[strformat, strutils, tables, options]

type
  SexpPathPartKind = enum
    spIndex
    spKey

  SexpPathPart = object
    case kind*: SexpPathPartKind
      of spIndex:
        index*: int

      of spKey:
        key*: string

  SexpPath* = seq[SexpPathPart]

  SexpMismatchKind* = enum
    smMissingKey
    smDifferentLiteral
    smDifferentSymbol
    smArrayLen
    smKindMismatch

  SexpMismatch* = object
    path*: SexpPath
    case kind*: SexpMismatchKind
      of smMissingKey:
        key*: string

      of smDifferentLiteral, smKindMismatch, smArrayLen, smDifferentSymbol:
        expected*, found*: SexpNode
        arraydiff*: tuple[target, input: seq[int]]

func part(key: string): SexpPathPart =
  SexpPathPart(key: key, kind: spKey)

func part(index: int): SexpPathPart =
  SexpPathPart(index: index, kind: spIndex)


func mismatch(path: SexpPath, key: string): SexpMismatch =
  SexpMismatch(kind: smMissingKey, key: key, path: path)

func mismatch(
    kind: SexpMismatchKind, path: SexpPath,
    expected, found: SexpNode
  ): SexpMismatch =

  result = SexpMismatch(kind: kind, path: path)
  result.expected = expected
  result.found = found


func diff*(target, input: SexpNode): seq[SexpMismatch] =
  proc aux(
      target, input: SexpNode,
      path: SexpPath,
      mismatches: var seq[SexpMismatch]
    ) =

    if target.kind != input.kind:
      mismatches.add mismatch(smKindMismatch, path, target, input)

    else:
      case target.kind:
        of SInt:
          if target.getNum() != input.getNum():
            mismatches.add mismatch(smDifferentLiteral, path, target, input)

        of SFloat:
          if target.getFNum() != input.getFNum():
            mismatches.add mismatch(smDifferentLiteral, path, target, input)

        of SString:
          if target.getStr() != input.getStr():
            mismatches.add mismatch(smDifferentLiteral, path, target, input)

        of SSymbol:
          if target.getSymbol() != input.getSymbol():
            mismatches.add mismatch(smDifferentSymbol, path, target, input)

        of SList:
          var
            inputKeys: Table[string, int]
            inputNonKeys, targetNonKeys: seq[int]

          for idx, item in pairs(input):
            if item.kind == SKeyword:
              inputKeys[item.getKey()] = idx

            else:
              inputNonKeys.add idx

          for idx, item in pairs(target):
            if item.kind == SKeyword:
              let key = item.getKey()
              if key in inputKeys:
                aux(item, input[inputKeys[key]], path & part(key), mismatches)

              else:
                mismatches.add mismatch(path, key)

            else:
              targetNonKeys.add idx

          if inputNonKeys.len != targetNonKeys.len:
            var mis =  mismatch(smArrayLen, path, target, input)
            mis.arraydiff = (targetNonKeys, inputNonKeys)
            mismatches.add mis

          for idx in 0 ..< min(inputNonKeys.len, targetNonKeys.len):
            aux(
              target[targetNonKeys[idx]],
              input[inputNonKeys[idx]],
              path & part(inputNonKeys[idx]),
              mismatches
            )

        of SCons:
          aux(target.car, input.car, path & part(0), mismatches)
          aux(target.cdr, input.cdr, path & part(1), mismatches)

        of SNil:
          discard

        of SKeyword:
          aux(target.value, input.value, path, mismatches)


  aux(target, input, @[], result)

func formatPath(path: SexpPath): string =
  if path.len == 0:
    result = "<root>"

  else:
    for part in path:
      case part.kind:
        of spIndex:
          result.add "[" & $part.index & "]"

        of spKey:
          result.add ":" & part.key

proc describeDiff(diff: seq[SexpMismatch]): string =
  for idx, mismatch in diff:
    if 0 < idx: result.add "\n"

    case mismatch.kind:
      of smKindMismatch:
        result.add "kind mismatch at path $# - expected $#, but got $#" % [
          formatPath(mismatch.path),
          $mismatch.expected.kind,
          $mismatch.found.kind,
        ]

      of smMissingKey:
        result.add "object at path $# misses key ':$#'" % [
          formatPath(mismatch.path),
          mismatch.key
        ]

      of smDifferentLiteral:
        result.add "literal mismatch at path $# - expected '$#', but got '$#'" % [
          formatPath(mismatch.path),
          $mismatch.expected,
          $mismatch.found
        ]

      of smDifferentSymbol:
        result.add "symbol mismatch at path $# - expected '$#', but got '$#'" % [
          formatPath(mismatch.path),
          $mismatch.expected,
          $mismatch.found
        ]

      of smArrayLen:
        result.add "array len mismatch at path $# - expected $# elements, but got $#" % [
          formatPath(mismatch.path),
          $mismatch.expected.len,
          $mismatch.found.len,
        ]

proc sdiff(target, input: string): Option[string] =
  let (target, input) = (target.parseSexp(), input.parseSexp())
  let diff = diff(target, input)
  if 0 < len(diff):
    return some diff.describeDiff()

for str in @[
  ("1", "2"),
  ("(:line 12 :col 10)", "(:line 30 :col 30)"),
  ("(Kind :expr 12)", "(Kind :expr 39)"),
  ("(Kind :expr 12)", "(Kind)"),
  ("(SymA :expr 12)", "(SymB :expr 12)")
]:
  let diff = sdiff(str[0], str[1])
  if diff.isSome():
    echo "```diff"
    echo "- ", str[0]
    echo "+ ", str[1]
    echo diff.get()
    echo "```\n"
