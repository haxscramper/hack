import sequtils, strformat, strutils, ropes, random, sugar,
       times, stats

type
  Ast = object
    sub: seq[Ast]

proc toString(a: Ast): string =
  if a.sub.len == 0:
    "-"
  else:
    "{" & a.sub.mapIt(it.toString()).join("") & "}"

proc toRope(a: Ast): Rope =
  if a.sub.len == 0:
    return rope("-")
  else:
    result = rope("{")
    for sub in a.sub:
      result.add sub.toRope()
    result.add "}"


proc makeTree(depth: int): Ast =
  if depth == 0:
    result = Ast()
  else:
    result.sub = collect(newSeq):
      for i in 0 .. 5:
        makeTree(depth - 1)

for depth in 0 .. 6:
  var stringTime: RunningStat
  var ropeTime: RunningStat
  var ropeBuilding: RunningStat

  for i in 0 .. 5:
    let tree = makeTree(depth)

    block: # string
      let parseStart = cpuTime()
      let val = tree.toString()

      stringTime.push(cpuTime() - parseStart)

    block: # rope
      block: # rope -> string
        let parseStart = cpuTime()
        let val = $tree.toRope()
        ropeTime.push(cpuTime() - parseStart)

      block: # only build
        let parseStart = cpuTime()
        let val = tree.toRope()
        ropeBuilding.push(cpuTime() - parseStart)

  echo &"string for depth      [{depth}][ns]: ", int(
    stringTime.mean() * 1000 * 1000)

  echo &"rope for depth        [{depth}][ns]: ", int(
    ropeTime.mean() * 1000 * 1000)

  echo &"rope build for  depth [{depth}][ns]: ", int(
    ropeBuilding.mean() * 1000 * 1000)
