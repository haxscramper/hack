import parseutils, strutils, macros, strscans, sequtils, macroutils, sugar

dumpTree:
  var matches: (int)
  scanf(" 1", matches[0])

macro scanftyped(input, pattern: string): untyped =
  var matchers: seq[string]
  for kind, value in ($pattern.toStrLit()).interpolatedFragments():
    echo kind, " ", value
    if kind == ikVar:
      matchers.add value

  var matches: NimNode =
    block:
      var tmp: seq[NimNode]
      for idx, str in matchers:
        tmp.add IdentDefs(
          "match" & $idx,
          (
            case str:
              of "i": ident "int"
              else: ident "float"
          ),
          Empty())

      VarSection(tmp)

  var call = Call(
    "scanf",
    @[input, pattern] &
    (block:
      collect(newSeq):
        for idx, _ in matchers:
          ident "match" & $idx)
  )

  result = quote do:
    `matches`
    `call`

  echo result.toStrLit()
  echo result.treeRepr()

if scanfTyped("12", "$i"):
  echo match0
