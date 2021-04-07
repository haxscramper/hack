import std/[json, macros, strformat, sequtils]

{.experimental: "caseStmtMacros".}

import fusion/matching

macro typecase(t: untyped): untyped =
  let expr = t[0]
  result = nnkWhenStmt.newTree()
  for branch in t:
    case branch:
      of OfBranch[until @types is StmtList(), @body]:
        result.add nnkElifBranch.newTree(
          mapIt(types, nnkInfix.newTree(ident("is"), expr, it)).
            foldl(nnkInfix.newTree(ident("or"), a, b)),
          body
        )

      of Else():
        result.add branch

      of Ident():
        discard


proc tjsonTag2[T](entry: T): string =
  typecase case entry:
    of seq, array:
      result = &"A<{tjsonTag(entry[0])}>"

    of tuple, object:
      result = "O"

    of SomeInteger:
      result = "d"

    of string:
      result = "s"

    of SomeFloat:
      result = "f"

    else:
      static:
        {.error: $typeof(entry).}

proc tjsonTag[T](entry: T): string =
  when entry is seq or entry is array:
    result = &"A<{tjsonTag(entry[0])}>"

  elif entry is tuple or entry is object:
    result = "O"

  elif entry is SomeInteger:
    result = "d"

  elif entry is string:
    result = "s"

  elif entry is SomeFloat:
    result = "f"

  else:
    static:
      {.error: $typeof(entry).}

proc toTJson[T](entry: T): JsonNode =
  when entry is object or entry is tuple:
    result = newJObject()
    for name, value in fieldPairs(entry):
      result[name & ":" & tjsonTag2(value)] = toTJson(value)

  elif entry is string:
    result = newJString(entry)

  elif entry is SomeFloat:
    result = newJFloat(entry)

  elif entry is SomeInteger:
    result = newJInt(entry)

  elif entry is seq or entry is array:
    result = newJArray()
    for item in items(entry):
      result.add toTJson(item)

  else:
    static:
      {.error: $typeof(entry).}

proc fromTJson*[T](res: var T, json: JsonNode) =
  when res is object or res is tuple:
    for name, value in fieldPairs(res):
      let key = name & ":" & tjsonTag(value)
      if key in json:
        fromTJson(value, json[key])

      else:
        echo "error"

  elif res is array:
    for idx, _ in mpairs(res):
      fromTJson(res[idx], json[idx])

  elif res is SomeInteger:
    assert json.kind == JInt
    res = json.getInt()

  elif res is string:
    assert json.kind == JString
    res = json.getStr()

  elif res is SomeFloat:
    assert json.kind == JFloat
    res = json.getFloat()

  else:
    static:
      {.error: $typeof(res).}


let inVal = (a: [1, 2], b: ("123", 0.23))
var outVal: typeof(inVal)
fromTJson(outVal, toTJson(inVal))

echo inVal
echo outVal
