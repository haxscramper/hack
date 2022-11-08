import
  std/[
    strutils,
    sets
  ]

proc printAnything[T](
    known: var HashSet[int],
    counter: var int,
    thing: T,
    res: var string
  ): int =
  when thing is ref or thing is ptr:
    if cast[int](thing) in known:
      result = cast[int](thing)
      return

    else:
      result = cast[int](thing)
      res.add "t$#[label=\"$#\"];\n" % [ $result, $typeof(thing) ]
      known.incl result

  else:
    result = counter
    res.add "t$#[label=\"$#\"];\n" % [ $result, $typeof(thing) ]
    inc counter

  when thing is ref or thing is ptr:
    for field, value in fieldPairs(thing[]):
      res.add "t$# -> t$#[label=\"$#\"];\n" % [
        $result, $printAnything(known, counter, value, res), field
      ]

  elif thing is object or thing is tuple:
    for field, value in fieldPairs(thing[]):
      res.add "t$# -> t$#[label=\"$#\"];\n" % [
        $result, $printAnything(known, counter, value, res), field
      ]

  elif thing is seq:
    var sub: seq[string]
    for idx, item in thing:
      res.add "t$# -> t$#_$# -> t$#;\n" % [
        $result, $result, $idx, $printAnything(known, counter, item, res)
      ]

      sub.add "t$#_$#" % [ $result, $idx ]

    if 0 < sub.len():
      res.add "{rank=same;$#;}\n" % sub.join(" -> ")

  else:
    res.add "t$#[label=\"$#='$#'\"];\n" % [
      $result, $typeof(thing), $thing
    ]

type
  Obj = ref object
    a: int
    b: float
    c: string
    sub: seq[Obj]

var res: string
var cnt: int
var known: HashSet[int]
var rec = Obj(sub: @[Obj(), Obj()])
rec.sub.add(rec)
discard printAnything(known, cnt, rec, res)

res = """
digraph G {
  node[fontname=consolas, shape=rect];
  label[fontname=consolas];
  spline=polyline;
  $#
}
""" % res

echo res
import std/os
"/tmp/res.dot".writeFile(res)
discard execShellCmd("dot -Tpng -o/tmp/res.png /tmp/res.dot")
