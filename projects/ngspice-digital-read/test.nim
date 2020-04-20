import parseutils, strutils, macros, strscans, sequtils, macroutils, sugar

import strscans
export strscans

func matcherType*[T](
  arg: proc(s: string, arg: var T, start: int): int
                   ): T =
    discard

macro tscanf*(input, pattNode: string): untyped =
  ## Statically typed `scanf` wrapper. Similar to `=~` template from
  ## `re` module the `matches` variable is implicitly declared.

  runnableExamples:
    proc matcher1(s: string, arg: var seq[string], start: int): int =
      arg = @["##", "$$"]
      return s.len - start

    if tscanf("12x12---%1,1,1,1", "$ix$i$+%${matcher1}"):
      echo matches[0] / 2, " = ", matches[2]," ", matches[3]

      assert declared(matches)
      assert type(matches[3]) is seq[string]
    else:
      assert not declared(matches)
      echo "does not match"

    assert not declared(matches)


  var matchers: seq[(InterpolatedKind, string)]
  var p = 0
  let pattern: string = $pattNode.toStrLit()
  while p < pattern.len:
    if pattern[p] == '$':
      inc p
      case pattern[p]
        of '$': discard
        of 'w', 'b', 'o', 'i', 'h', 'f', '+', '*':
          matchers.add (ikVar, $pattern[p])
          inc p
        of '{':
          inc p
          var nesting = 0
          let start = p
          while true:
            case pattern[p]
              of '{': inc nesting
              of '}':
                if nesting == 0: break
                dec nesting
              of '\0': error("expected closing '}'")
              else: discard
            inc p
          let expr = pattern.substr(start, p-1)
          matchers.add (ikExpr, expr)
        else:
          inc p
    else:
      inc p

  let matches: NimNode =
    block:
      var tupleType: seq[NimNode]
      for idx, str in matchers:
        if str[0] == ikVar:
          echo str
          tupleType.add Ident(
            case str[1][0]:
              of 'i', 'o', 'b', 'h': "int"
              of 'f': "float"
              of '*', '+': "string"
              else: "float"
          )
        else:
          tupleType.add Call(
            "type", Call("matcherType", ident str[1]))

      tupleType.add ident("int")
          # echo getType(Call("matcherType", ident str[1]))


      superQuote do:
        var matches {.inject.}: `Par(tupleType)`

  var call = Call(
    "scanf",
    @[input, pattNode] &
    (block:
      collect(newSeq):
        for idx, _ in matchers:
          BracketExpr(ident "matches", newIntLitNode(idx)))
  )

  result = quote do:
    `matches`
    `call`

  echo result.toStrLit()
