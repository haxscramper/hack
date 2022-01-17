import std/[json, strformat, strutils]

type
  JsonPathPartKind = enum
    jpIndex
    jpKey

  JsonPathPart = object
    case kind*: JsonPathPartKind
      of jpIndex:
        index*: int

      of jpKey:
        key*: string

  JsonPath* = seq[JsonPathPart]

  JsonMismatchKind* = enum
    jmMissingKey
    jmDifferentLiteral
    jmArrayLen
    jmKindMismatch

  JsonMismatch* = object
    path*: JsonPath
    case kind*: JsonMismatchKind
      of jmMissingKey:
        key*: string

      of jmDifferentLiteral, jmArrayLen, jmKindMismatch:
        expected*, found*: JsonNode

func part(key: string): JsonPathPart =
  JsonPathPart(key: key, kind: jpKey)

func part(index: int): JsonPathPart =
  JsonPathPart(index: index, kind: jpIndex)


func mismatch(path: JsonPath, key: string): JsonMismatch =
  JsonMismatch(kind: jmMissingKey, key: key, path: path)

func mismatch(
    kind: JsonMismatchKind, path: JsonPath,
    expected, found: JsonNode
  ): JsonMismatch =

  result = JsonMismatch(kind: kind, path: path)
  result.expected = expected
  result.found = found


func diff*(target, input: JsonNode): seq[JsonMismatch] =
  proc aux(
      target, input: JsonNode,
      path: JsonPath,
      mismatches: var seq[JsonMismatch]
    ) =

    if target.kind != input.kind:
      mismatches.add mismatch(jmKindMismatch, path, target, input)

    else:
      case target.kind:
        of JInt:
          if target.getInt() != input.getInt():
            mismatches.add mismatch(jmDifferentLiteral, path, target, input)

        of JFloat:
          if target.getFloat() != input.getFloat():
            mismatches.add mismatch(jmDifferentLiteral, path, target, input)

        of JString:
          if target.getStr() != input.getStr():
            mismatches.add mismatch(jmDifferentLiteral, path, target, input)

        of JBool:
          if target.getBool() != input.getBool():
            mismatches.add mismatch(jmDifferentLiteral, path, target, input)

        of JNull:
          discard

        of JArray:
          if target.len != input.len:
            mismatches.add mismatch(jmArrayLen, path, target, input)

          for idx in 0 ..< min(target.len, input.len):
            aux(target[idx], input[idx], path & part(idx), mismatches)

        of JObject:
          for key in keys(target):
            if key in input:
              aux(target[key], input[key], path & part(key), mismatches)

            else:
              mismatches.add mismatch(path, key)

  aux(target, input, @[], result)

func formatPath(path: JsonPath): string =
  if path.len == 0:
    result = "<root>"

  else:
    for part in path:
      case part.kind:
        of jpIndex:
          result.add "[" & $part.index & "]"

        of jpKey:
          result.add "." & part.key

proc describeDiff(diff: seq[JsonMismatch]): string =
  for idx, mismatch in diff:
    if 0 < idx: result.add "\n"

    case mismatch.kind:
      of jmKindMismatch:
        result.add "kind mismatch at path $# - expected $#, but got $#" % [
          formatPath(mismatch.path),
          $mismatch.expected.kind,
          $mismatch.found.kind,
        ]

      of jmMissingKey:
        result.add "object at path $# misses key $#" % [
          formatPath(mismatch.path),
          mismatch.key
        ]

      of jmDifferentLiteral:
        result.add "literal mismatch at path $# - expected '$#', but got '$#'" % [
          formatPath(mismatch.path),
          $mismatch.expected,
          $mismatch.found
        ]

      of jmArrayLen:
        result.add "array len mismatch at path $# - expected $# elements, but got $#" % [
          formatPath(mismatch.path),
          $mismatch.expected.len,
          $mismatch.found.len,
        ]

proc jdiff(target, input: string) =
  let (target, input) = (target.parseJson(), input.parseJson())
  let diff = diff(target, input)
  echo diff.describeDiff()

jdiff("1", "2")
jdiff(
  """ {"line": 12, "col": 30} """,
  """ {"line": 12, "col": 39} """
)
