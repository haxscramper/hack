
func toRGB*(color: TermColorBg | TermColorFg): tuple[r, g, b: range[0 .. 5]] =
  ## Split 256-color value into red, green and blue channels. Note - this
  ## function expects 256-color codes, regular 16-bit values don't have
  ## mapping.
  let col = color.uint8 - offset256
  result.r = col div scaleRed
  result.g = (col div scaleGreen) mod 6
  result.b = col mod 6


func toHtmlColor*(color: TermColorFg | TermColorBg): string =
  if color.int in 0 .. 16:
    case ForegroundColor(color.int + low(ForegroundColor).int):
      of fgRed: result     = "Crimson"
      of fgBlue: result    = "MediumBlue"
      of fgBlack: result   = "black"
      of fgMagenta: result = "DarkViolet"
      of fgWhite: result   = "white"
      of fgYellow: result  = "Gold"
      of fgGreen: result   = "ForestGreen"
      of fgCyan: result    = "DarkCyan"
      else: result = "none"

      # of fgRed: result     = "red"
      # of fgBlue: result    = "blue"
      # of fgBlack: result   = "black"
      # of fgMagenta: result = "magenta"
      # of fgWhite: result   = "white"
      # of fgYellow: result  = "yellow"
      # of fgGreen: result   = "green"
      # of fgCyan: result    = "cyan"
      # else: result = "none"

  else:
    result = "#"
    # This is not entirely accurate, but we can sacrifice some color
    # representation quality
    if color.uint8 in offsetGray .. max256:
      let val = uint8((color.uint8 - offsetGray).float * (256.0 / 24.0))
      result.add toHex(val)
      result.add toHex(val)
      result.add toHex(val)

    else:
      let (r, g, b) = color.toRGB()
      result.add toHex(uint8(r.float * (256.0 / 6.0)))
      result.add toHex(uint8(g.float * (256.0 / 6.0)))
      result.add toHex(uint8(b.float * (256.0 / 6.0)))

func openHtml(s: ColStyle): string =
  result = "<span style=\""
  if not s.fg.isDefault():
    result.add "color:" & s.fg.toHtmlColor()
    result.add ";"

  if not s.bg.isDefault():
    result.add "background-color:" & s.bg.toHtmlColor()
    result.add ";"

  result.add "\">"

func closeHtml(): string = "</span>"

func toHtml*(rune: ColRune, color: bool = true): string =
  ## Generate HTML with
  let s = rune.style
  if color and (
    (not s.fg.isDefault()) or
    (not s.bg.isDefault()) or
    (s.style.len != 0)
  ):
    result.add openHtml(s)
    result.add $rune.rune
    result.add closeHtml()

  else:
    result = $rune.rune

func toHtml*(runes: seq[ColRune], color: bool = true): string =
  if color and 0 < len(runes):
    var prev = runes[0].style
    result.add openHtml(prev)
    result.add $runes[0].rune
    for rune in runes[1..^1]:
      if rune.style != prev:
        result.add closeHtml()
        result.add openHtml(rune.style)

      result.add $rune.rune
      prev = rune.style

    result.add closeHtml()

  else:
    for rune in runes:
      result.add $rune.rune

func toHtml*(text: ColText, color: bool = true): string =
  toHtml(text.runes, color)


when isMainModule:
  echo "r" + fgMagenta & "d"
  echo "red" + fgRed

  for (bg, fg) in zip(BackgroundColor.toSeq(), ForegroundColor.toSeq()):
    var buf: string
    for style in Style:
      buf.add $("[#]" + fg + style)
      buf.add $("[#]" + bg + style)
      buf.add " "

    echo buf

  var buf: ColText
  for gray in 0 .. 23:
    buf.add &"[{gray}]" + gray.termBg()

  buf.add "\n"
  for r in 0 .. 5:
    for g in 0 .. 5:
      for b in 0 .. 5:
        buf.add &"[{r} {g} {b}]" + termBg(r, g, b)
      buf.add "\n"
    buf.add "\n"

  echo buf

  echo "red" + fgRed & "blue" + fgBlue

  import std/[macros, os]

  proc treeRepr1*(
      pnode: NimNode,
      maxdepth: int = 120,
      lineInfo: bool = false
    ): ColText =
    coloredResult()

    proc aux(n: NimNode, level: int, idx: seq[int]) =
      addIndent(level)
      if isNil(n):
        add "<nil>" + fgRed
        return

      if level > maxdepth:
        add " ..."
        return

      add $n.kind
      if lineInfo:
        let info = n.lineInfoObj()
        add "@"
        add splitFile(info.filename).name + fgBlue
        add "/"
        add $info.line + fgCyan
        add ":"
        add $info.column + fgCyan
        add " "

      case n.kind:
        of nnkStrLit .. nnkTripleStrLit:
          add " \"" & n.strVal() + fgYellow & "\""

        of nnkCharLit .. nnkUInt64Lit :
          add " " & $n.intVal + fgCyan

        of nnkFloatLit .. nnkFloat128Lit:
          add " " & $n.floatVal + fgMagenta

        of nnkIdent:
          add " " & n.strVal() + fgCyan

        of nnkSym:
          add " "
          add ($n.symKind())[3..^1] + fgBlue
          add " "
          add n.strVal() + fgBlue

        of nnkCommentStmt:
          let lines = split(n.strVal(), '\n')
          if lines.len > 1:
            add "\n"
            for idx, line in pairs(lines):
              if idx != 0:
                add "\n"

              addIndent(level)
              add line + fgYellow

          else:
            add n.strVal() + fgYellow

        else:
          if n.len > 0:
            add "\n"

          for newIdx, subn in n:
            aux(subn, level + 1, idx & newIdx)
            if level + 1 > maxDepth:
              break

            if newIdx < n.len - 1:
              add "\n"



    aux(pnode, 0, @[])
    endResult()

  macro reprt(node: typed) =
    echo node.treeRepr1()

  reprt:
    echo 123
