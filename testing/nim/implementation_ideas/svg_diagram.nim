import hmisc/core/all
import std/[
  colors,
  options,
  unicode,
  strutils,
  strformat,
  sequtils,
  math
]

type
  Style = object
    fg: Color
    bg: Color
    box: Option[Color]

  Span = object
    text: seq[Rune]
    style: Style
    pad: tuple[left, right, top, down: int]

  ShapeKind = enum
    skRect

  Shape = object
    pos: tuple[x, y: int]
    case kind*: ShapeKind
      of skRect:
        size: tuple[h, w: int]

  Image = object
    charsize: tuple[h, w: int]
    textpad: tuple[x, y: int]
    text: seq[seq[Span]]
    shapes: seq[Shape]
    bg: Color

func style(
    fg: Color = colBlack, bg: Color = colWhite,
    box: Option[Color] = none(Color)): Style =
  Style(fg: fg, bg: bg, box: box)

func span(
    text: string,
    style: Style = style(),
    pad: tuple[left, right, top, down: int] = (0, 0, 0, 0)): Span =

  Span(text: toRunes(text), style: style, pad: pad)

func addRow(image: var Image, span: Span) =
  image.text.add @[span]

func addRow(image: var Image) =
  image.text.add @[]

func add(image: var Image, span: Span | seq[Span]) =
  image.text.last().add(span)

func image(text: seq[seq[Span]]): Image =
  Image(text: text)

func toSVG(image: Image): string =
  let (charh, charw) = image.charsize

  result.addf(
    """
<svg width="$#" height="$#">
<style>
  text {
    font-family: "monospace";
    font-size: 4em;
  }
</style>
""",
    charw * image.text.mapIt(mapIt(it, it.text.len()).sum()).sum() + 120,
    charh * image.text.len() + 120
  )

  var xPos, yPos = 0

  for row in image.text:
    for col in row:
      let (left, right, top, down) = col.pad
      var style: string
      if col.style.box.canGet(box):
        style.addf("fill:$#;stroke:$#", col.style.bg, box)

      elif col.style.bg != image.bg:
        style.addf("fill:$#", col.style.bg)

      if not style.empty():
        result.addf(
          """  <rect x="$#" y="$#" rx="6" ry="6" width="$#" height="$#" style="$#"/>$#""",
          xPos * charw + image.textpad.x + left,
          yPos * charh + image.textpad.y + top,
          charw * col.text.len,
          charh,
          style,
          "\n"
        )

      result.addf(
        "  <text x=\"$#\" y=\"$#\" style=\"$#\">$#</text>\n",
        xPos * charw + left,
        (yPos + 1) * charh + top,
        "fill:$#" % [$col.style.fg],
        col.text
      )

      inc xPos, col.text.len()
    inc yPos
    xPos = 0

  result.addf("</svg>")

func `div`(c: Color, i: int): Color =
  let (r, g, b) = c.extractRgb()
  return rgb(r div i, g div i, b div i)

proc main() =
  var image = Image(
    charsize: (h: 50, w: 29),
    textpad: (x: 0, y: 10),
    bg: colWhite
  )

  image.addRow()
  image.add span("?")
  image.addRow()
  for col in [colRed, colCyan, colBlue]:
    image.add span(
      $col,
      style(bg = col, box = some col), pad = (2,2,0,0))

  image.add span("same row")
  image.addRow()
  image.add span("Regular text")
  image.addRow()
  image.add span("{", style(fg = colWhite, bg = colGreen))
  image.add span("arg")
  image.add span("}", style(fg = colWhite, bg = colGreen))

  let text = image.toSVG()
  echo text
  writeFile("/tmp/res.svg", text)


main()
