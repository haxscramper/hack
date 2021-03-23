type
  Match = object
    name: string
    sub: seq[Match]

  AccsElem = object
    name: string

  Path = seq[AccsElem]

static:
  proc aux(sub: Match, path: Path) =
    echo path
    for elem in sub.sub:
      let newPath = path & @[AccsElem(name: elem.name)]
      echo newPath
      aux(elem, newPath)

  aux(
    Match(name: "test", sub: @[
      Match(name: "test1"),
      Match(name: "test2"),
    ]),
    @[]
  )
