import std/macros

type
  Pos* = object
    line*: int
    column*: int

proc toPos(info: LineInfo): Pos =
  Pos(line: info.line, column: info.column)

proc startPos(node: NimNode): Pos =
  case node.kind:
    of nnkNone .. nnkNilLit:
      result = toPos(node.lineInfoObj())

    else:
      result = node[0].startPos()

proc finishPos(node: NimNode): Pos =
  case node.kind:
    of nnkNone .. nnkNilLit:
      result = toPos(node.lineInfoObj())
      result.column += len($node) - 1

    else:
      if len(node) > 0:
        var idx = len(node) - 1
        while idx >= 0 and node[idx].kind in {nnkEmpty}:
          dec idx

        if idx >= 0:
          result = node[idx].finishPos()

        else:
          result = toPos(node.lineInfoObj())

      else:
        result = toPos(node.lineInfoObj())


macro nodeExtent(node: untyped): untyped =
  echo node.startPos(), " ", node.finishPos()

nodeExtent:
  for i in 0 .. 2:
    # test
    echo 23
    #    ^^
    #    |
    #    Explanation for number 23
