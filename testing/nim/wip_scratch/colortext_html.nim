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
      of fgRed: result     = "red"
      of fgBlue: result    = "blue"
      of fgBlack: result   = "black"
      of fgMagenta: result = "magenta"
      of fgWhite: result   = "white"
      of fgYellow: result  = "yellow"
      of fgGreen: result   = "green"
      of fgCyan: result    = "cyan"
      else: result = "none"

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
  echo toHtml("red" + fgRed & "green" + fgGreen)
  echo toHtml("red" + fgRed + bgGreen)
  echo toHtml("red" + termFg(1, 2, 3) + bgGreen)
