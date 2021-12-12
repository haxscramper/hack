## Nim reimplementation of layout algorithm from
## https://github.com/google/rfmt.

# Thanks to nim being syntactically close to python this is mostly
# just blatant copy-paste of python code with added type annotations.

## .. include:: blockfmt-doc.rst

import std/[
  strutils, sequtils, macros, tables, strformat,
  lenientops, options, hashes, math, sugar, streams,
  intsets
]

const
  infty = 1024 * 1024 * 1024 * 1024

func inf(a: int): bool = (infty - 4096 <= a) and (a <= infty + 4096)

func `*`(a: SomeNumber, b: bool): SomeNumber = (if b: a else: 0)
func get[T](inseq: seq[Option[T]]): seq[T] =
  for elem in inseq:
    if elem.isSome():
      result.add elem.get()

type
  StringAlignDirection* = enum
    sadLeft
    sadRight
    sadCenter

iterator zip*[T1, T2, T3, T4, T5](
    s1: seq[T1],
    s2: seq[T2],
    s3: seq[T3],
    s4: seq[T4],
    s5: seq[T5]
  ): tuple[v1: T1, v2: T2, v3: T3, v4: T4, v5: T5] =

  for idx in 0 ..< min([s1.len, s2.len, s3.len, s4.len, s5.len]):
    yield (s1[idx], s2[idx], s3[idx], s4[idx], s5[idx])

iterator rmpairs*[T](s: var seq[T]): (int, var T) =
  ## Iterate over mutable sequence starting from the right
  for idx in countdown(s.len - 1, 0):
    yield (idx, s[idx])


func escapeStrLit*(input: string): string =
  input.multiReplace([
    ("\"", "\\\""),
    ("\n", "\\n"),
    ("\\", "\\\\")
  ])

#*************************************************************************#
#****************************  Format policy  ****************************#
#*************************************************************************#
#===========================  Type definition  ===========================#
type
  LayoutElementKind = enum
    lekString
    lekNewline
    lekNewlineSpace
    lekLayoutPrint

  LayoutElement = ref object
    ## An element of a layout object - a directive to the console.
    ##
    ## This class sports a collection of static methods, each of which
    ## returns an anonymous function invoking a method of the console
    ## to which it is applied.
    ##
    ## Refer to the corresponding methods of the LytConsole class for
    ## descriptions of the methods involved.
    id {.requiresinit.}: int
    kind: LayoutElementKind
    text: LytStr ## Layout element text
    indent: bool
    spaceNum: int
    layout: Layout

  Layout* = ref object
    ## An object containing a sequence of directives to the console.
    elements: seq[LayoutElement]

  LytSolution* = ref object
    id {.requiresinit.}: int
    ## A Solution object effectively maps an integer (the left margin at
    ## which the solution is placed) to a layout notionally optimal for
    ## that margin, together with cost information used to evaluate the
    ## layout. For compactness, the map takes the form of a
    ## piecewise-linear cost function, with associated layouts.

    knots: seq[int] ## a list of ints, specifying the margin settings at
    ## which the layout changes. Note that the first knot is required to be
    ## 0.
    spans: seq[int] ## a list of ints, giving for each knot, the width of
                    ## the corresponding layout in characters.
    intercepts: seq[float] ## constant cost associated with each knot.
    gradients: seq[float] ## at each knot, the rate with which the layout
                          ## cost increases with an additional margin
                          ## indent of 1 character.
    layouts*: seq[Layout] ## the Layout objects expressing the optimal
                          ## layout between each knot.
    index: int

  LytBlockKind* = enum
    bkText ## Single line text block
    bkLine ## Horizontally stacked lines
    bkChoice ## Several alternating layouts
    bkStack ## Vertically stacked layouts
    bkWrap ## Mulitple blocks wrapped to create lowerst-cost layout
    bkVerb ## Multiple lines verbatim
    bkEmpty ## Empty layout block - ignored by `add` etc.

  LytStr* = object
    text*: string

  LytBlock* = ref object
    layoutCache: Table[Option[LytSolution], Option[LytSolution]]
    isBreaking* {.requiresinit.}: bool ## Whether or not this block should end the line
    breakMult* {.requiresinit.}: int ## Local line break cost change

    minWidth* {.requiresinit.}: int
    hasInnerChoice*: bool

    id {.requiresinit.}: int

    case kind*: LytBlockKind
      of bkVerb:
        textLines*: seq[LytStr] ## Multiple lines of text
        firstNl*: bool ## Insert newline at the block start

      of bkText:
        text*: LytStr ## A single line of text, free of carriage
                      ## returs etc.

      of bkWrap:
        prefix*: Option[string]
        sep*: string ## Separator for block wraps
        wrapElements*: seq[LytBlock]

      of bkStack, bkChoice, bkLine:
        elements*: seq[LytBlock]

      of bkEmpty:
        discard

  LytFormatPolicy = object
    breakElementLines: proc(
      blc: seq[seq[LytBlock]]): seq[seq[LytBlock]] ## Hook

  LytOptions* = object
    leftMargin*: int ## position of the first right margin. Expected `0`
    rightMargin*: int ## position of the second right margin. Set for `80`
                      ## to wrap on default column limit.
    leftMarginCost*: float ## cost (per character) beyond margin 0.
                           ## Expected value `~0.05`
    rightMarginCost*: float ## cost (per character) beyond margin 1. Should
                            ## be much higher than `c0`. Expected value
                            ## `~100`
    linebreakCost*: int ## cost per line-break
    indentSpaces*: int ## spaces per indent
    # adj_comment: int
    # adj_flow: int #
    # adj_call: int
    # adj_arg: int
    cpack*: float ## cost (per element) for packing justified layouts.
                 ## Expected value `~0.001`
    format_policy*: LytFormatPolicy


  OutConsole* = object
    leftMargin: int
    rightMargin: int
    hPos: int
    margins: seq[int]
    outStr: string

proc margin(buf: OutConsole): int = buf.margins[^1]

func `$`(s: LytStr): string = $s.text
func len*(s: LytStr): int = s.text.len
func lytStr(s: string): LytStr = LytStr(text: s)

proc printStr(buf: var OutConsole, str: LytStr) =
  buf.outStr.add str.text
  buf.hPos += str.len

proc printStr(buf: var OutConsole, str: string) =
  buf.outStr.add str
  buf.hPos += str.len

proc printSpace(buf: var OutConsole, n: int) =
  buf.printStr(repeat(" ", n))

proc printNewline(buf: var OutConsole, indent: bool = true) =
  buf.printStr("\n")
  buf.hPos = 0
  if indent:
    buf.printSpace(buf.margin())

proc printNewlineSpace(buf: var OutConsole, n: int) =
  buf.printNewline()
  buf.printSpace(n)

proc treeRepr*(self: Layout, level: int = 0): string =
  var r = addr result
  proc add(s: string) = r[].add s

  proc aux(lyt: Layout, l: int)
  proc aux(lyt: LayoutElement, l: int) =
    add repeat("  ", l)
    add &"id {lyt.id} "
    case lyt.kind:
      of lekString:
         add "[text] 《"
         add lyt.text.text
         add "》"

      of lekNewline:
        add "[newline]"

      of lekNewlineSpace:
        add "[newline][space]"

      of lekLayoutPrint:
        add "[lyt]\n"
        aux(lyt.layout, l + 1)

  proc aux(lyt: Layout, l: int) =
    for idx, elem in pairs(lyt.elements):
      if not idx == 0:
        add "\n"
      aux(elem, l)

  aux(self, level)

proc treeRepr*(self: LytSolution, level: int = 0): string =
  result.add "[lyt solution]\n"
  for lyt in self.layouts:
    result.add "  [lyt]\n"
    result.add treeRepr(lyt, 2)
    result.add "\n"

proc printOn*(self: Layout, buf: var OutConsole) =
  buf.margins.add buf.hPos
  for elem in self.elements:
    case elem.kind:
      of lekString: buf.printStr(elem.text)
      of lekNewline: buf.printNewline(elem.indent)
      of lekNewlineSpace: buf.printNewlineSpace(elem.spaceNum)
      of lekLayoutPrint: elem.layout.printOn(buf)

  discard buf.margins.pop()

proc write*(stream: Stream | File, self: Layout, indent: int = 0) =
  if indent == 0:
    for elem in self.elements:
      stream.write(elem.text)

  else:
    for elem in self.elements:
      for ch in elem.text:
        if ch == '\n':
          stream.write " ".repeat(indent)

        stream.write ch


proc debugOn*(self: Layout, buf: var string): void =
  for elem in self.elements:
    buf &= $elem.text

proc `$`*(le: LayoutElement): string = $le.text

proc `$`*(lt: Layout): string =
  lt.debugOn(result)



proc `$`*(sln: LytSolution): string =
  result &= "<"
  var idx: int = 0
  for s in zip(
    sln.knots, sln.spans, sln.intercepts, sln.gradients, sln.layouts
  ):
    if idx > 0:
      result &= ", "

    result &= &"{s[0]}/({s[1]}, {s[2]:.2}, {s[3]}, \"{$s[4]}\")"
    inc idx

  result &= ">"

proc `$`*(sln: Option[LytSolution]): string =
  if sln.isSome(): return $sln.get()

proc `$`*(blc: LytBlock): string =
  result = $blc.minWidth & " "
  result &= (
    case blc.kind:
      of bkText:
        "T[" & (if blc.isBreaking: "*" else: "") & &"\"{blc.text}\"]"

      of bkStack:
        "V[" & blc.elements.mapIt($it).join(" ↕ ") & "]"

      of bkLine:
        "H[" & blc.elements.mapIt($it).join(" ↔ ") & "]"

      of bkChoice:
        "(" & blc.elements.mapIt($it).join(" ? ") & ")"

      of bkWrap:
        "[" & blc.wrapElements.mapIt($it).join(" ") & "]"

      of bkVerb:
        $blc.textLines[0].text & "..."

      of bkEmpty:
        "<empty>"
  )
      # &""">>{blc.textLines.join("⮒")}<<"""



func treeRepr*(inBl: LytBlock): string =
  func aux(bl: LytBlock, level: int): string =
    let name =
      case bl.kind:
        of bkLine: "L"
        of bkChoice: "C"
        of bkText: "T"
        of bkWrap: "W"
        of bkStack: "S"
        of bkVerb: "V"
        of bkEmpty: "E"

    let pref = align(
      name & " ", level * 2) &
        &"brk: {bl.isBreaking} " &
        &"mul: {bl.breakMult} min: {bl.minWidth} id: {bl.id}"

    let pref2 = repeat(" ", level * 2 + 2)

    result = pref2 & pref

    case bl.kind:
      of bkLine, bkChoice, bkStack, bkWrap:
        result &= "\n"
        for elem in items(
          if bl.kind == bkWrap: bl.wrapElements else: bl.elements
        ):

          result &= elem.aux(level + 1)

      of bkText:
        result &= "〈" & $bl.text.text & "〉\n"

      of bkEmpty:
        result &= "<empty>"

      of bkVerb:
        result &= "\n"
        for line in items(bl.textLines):
          result &= pref2 & repeat("  ", clamp(level - 1, 0, high(int))) & "  〚" & $line & "〛\n"

  return aux(inBl, 0)





#*************************************************************************#
#************************  LytOptions configuration  ************************#
#*************************************************************************#


func hash(elem: LayoutElement): Hash = hash(elem.text.text)
func hash(lyt: Layout): Hash = hash(lyt.elements)

func hash(sln: Option[LytSolution]): Hash =
  if sln.isNone():
    return
  else:
    return sln.get.id
    # let sln = sln.get()
    # result = !$(
    #   hash(sln.knots) !&
    #   hash(sln.spans) !&
    #   hash(sln.intercepts) !&
    #   hash(sln.gradients) !&
    #   hash(sln.layouts) !&
    #   hash(sln.index)
    # )

#*************************************************************************#
#*******************************  Layout  ********************************#
#*************************************************************************#

func getSId(): int =
  var slnId {.global.}: int
  {.cast(noSideEffect).}:
    inc slnId
    result = slnId

func lytString(s: string): LayoutElement =
  LayoutElement(text: lytStr(s), kind: lekString, id: getSId())

func lytString(s: LytStr): LayoutElement =
  LayoutElement(text: s, kind: lekString, id: getSId())

func lytNewline(indent: bool = true): LayoutElement =
  LayoutElement(indent: indent, kind: lekNewline, id: getSId())

func lytNewlineSpace(n: int): LayoutElement =
  LayoutElement(spaceNum: n, kind: lekNewlineSpace, id: getSId())

proc lytPrint(lyt: Layout): LayoutElement =
  LayoutElement(kind: lekLayoutPrint, layout: lyt, id: getSId())

proc getStacked(layouts: seq[Layout]): Layout =
  ## Return the vertical composition of a sequence of layouts.

  ## Args:
  ##   layouts: a sequence of Layout objects.
  ## Returns:
  ##   A new Layout, stacking the arguments.
  var lElts: seq[LayoutElement]
  for l in layouts:
    for e in l.elements:
      lElts.add e

    lElts.add lytNewLine()

  return Layout(elements: lElts[0 .. ^2])  # Drop the last NewLine()

func initLayout(elems: seq[LayoutElement]): Layout =
  Layout(elements: elems)

#*************************************************************************#
#******************************  LytSolution  *******************************#
#*************************************************************************#

proc initSolution(
    knots: seq[int], spans: seq[int], intercepts: seq[float],
    gradients: seq[float], layouts: seq[Layout]): LytSolution =
  result = LytSolution(
    knots: knots, spans: spans, intercepts: intercepts,
    gradients: gradients, layouts: layouts, id: getSId())


#===========================  Helper methods  ============================#
func reset(self: var LytSolution) =
  ## Begin iteration.
  self.index = 0

func advance(self: var LytSolution) =
  ## Advance to the next knot.
  self.index += 1

func retreat(self: var LytSolution) =
  ## Move back a knot.
  self.index -= 1

func curKnot(self: LytSolution): int =
  ## The currently indexed knot.
  return self.knots[self.index]

func curSpan(self: LytSolution): int =
  return self.spans[self.index]

func curIntercept(self: LytSolution): float =
  return self.intercepts[self.index]

func curGradient(self: LytSolution): float =
  return self.gradients[self.index]

func curLayout(self: LytSolution): Layout = self.layouts[self.index]
func curIndex(self: LytSolution): int = self.index

func curValueAt(self: LytSolution, margin: int): float =
  ## The value (cost) extrapolated for margin m from the current knot.
  # Since a LytSolution's cost is represented by a piecewise linear function,
  # the extrapolation in this case is linear, from the current knot.
  return self.curIntercept() + self.curGradient() * float(
    margin - self.curKnot())

func nextKnot(self: LytSolution): int =
  ## The knot after the once currently indexed.
  if self.index + 1 >= self.knots.len:
    infty
  else:
    self.knots[self.index + 1]

proc moveToMargin(self: var LytSolution, margin: int) =
  ## Adjust the index so m falls between the current knot and the next.
  if self.curKnot() > margin:
    while self.curKnot() > margin:
      self.retreat()
  else:
    while self.nextKnot() <= margin and self.nextKnot() != infty:
      self.advance()
      # info "Advancing to position", self.curIndex(),
      #   "next knot is", self.nextKnot(), "margin is", margin,
      #   self.nextKnot() <= margin


#==========================  LytSolution factory  ===========================#

type
  LytSolutionFactory = object
    ## A factory object used to construct new LytSolution objects.
    ##
    ## The factory performs basic consistency checks, and eliminates
    ## redundant segments that are linear extrapolations of those that
    ## precede them.
    entries: seq[tuple[
      knot, span: int, intercept, gradient: float, lyt: Layout]]

proc add(
  self: var LytSolutionFactory,
  knot, span: int, intercept, gradient: float, layout: Layout): void =
  ## Add a segment to a LytSolution under construction.
  # debug "Add layout", layout, "to solution"
  if self.entries.len > 0:
    # Don't add a knot if the new segment is a linear extrapolation of
    # the last.
    let (k_last, s_last, i_last, g_last, _) = self.entries[^1]
    if (span == s_last and gradient == g_last and
        i_last + (knot - k_last) * g_last == intercept):
      return

  if knot < 0 or span < 0 or intercept < 0 or gradient < 0:
    raiseAssert(
      "Internal error: bad layout: " &
        &"(k {knot}, s {span}, i {intercept}, g {gradient})")

  self.entries.add (knot, span, intercept, gradient, layout)

proc makeSolution(self: LytSolutionFactory): LytSolution =
  ## Construct and return a new LytSolution with the data in this
  ## object
  new(result)
  for (k, s, i, g, l) in self.entries:
    result.knots.add k
    result.spans.add s
    result.intercepts.add i
    result.gradients.add g
    result.layouts.add l

#=====================  LytSolution manipulation logic  =====================#


proc minSolution(solutions: seq[LytSolution]): Option[LytSolution] =
  ## Form the piecewise minimum of a sequence of LytSolutions.

  ## Args:
  ##   solutions: a non-empty sequence of LytSolution objects
  ## Returns:
  ##   values LytSolution object whose cost is the piecewise minimum of the LytSolutions
  ##   provided, and which associates the minimum-cost layout with each piece.
  # debug "Minimal solution out of #", solutions.len
  if len(solutions) == 1:
    return some(solutions[0])

  var
    factory: LytSolutionFactory
    solutions = solutions

  for s in mitems(solutions):
    s.reset()

  let n = len(solutions)
  var
    k_l = 0
    last_i_min_soln = -1  # Index of the last minimum solution
    last_index = -1  # Index of the current knot in the last minimum
    # solution Move through the intervals [k_l, k_h] defined by the
    # glb of the partitions defined by each of the solutions.

  while k_l < infty:
    let
      k_h = min(solutions.map(nextKnot)) - 1
      gradients = solutions.map(curGradient)

    while true:
      let values = solutions.mapIt(it.curValueAt(k_l))
      # Use the index of the corresponding solution to break ties.
      let (min_value, min_gradient, i_min_soln) =
        (0 ..< n).mapIt((values[it], gradients[it], it)).min()

      let min_soln = solutions[i_min_soln]
      if i_min_soln != last_i_min_soln or min_soln.curIndex() != last_index:
        # Add another piece to the new LytSolution
        factory.add(
          k_l,
          min_soln.curSpan(),
          min_value,
          min_gradient,
          min_soln.curLayout()
        )

        last_i_min_soln = i_min_soln
        last_index = min_soln.curIndex()
      # It's possible that within the current interval, the minimum
      # solution may change, should a solution with a lower initial
      # value but greater gradient surpass the value of one with a
      # higher initial value but lesser gradient. In such instances,
      # we need to add an extra piece to the new solution.
      let distances_to_cross = collect(newSeq):
        for i in 0 ..< n:
          if gradients[i] < min_gradient:
            ceil((values[i] - min_value) / (min_gradient - gradients[i]))

      # Compute positions of all crossovers in [k_l, k_h]
      let crossovers = collect(newSeq):
        for d in distances_to_cross:
          if k_l + d <= k_h:
            k_l + d

      if crossovers.len > 0:  # Proceed to crossover in [k_l, k_h]
        k_l = min(crossovers).int # XXXX
      else:  # Proceed to next piece
        k_l = k_h + 1
        if k_l < infty:
          for s in mitems(solutions):
            s.moveToMargin(k_l)
        break

  return some factory.makeSolution()

proc vSumSolution(solutions: seq[LytSolution]): LytSolution =
  ## The layout that results from stacking several LytSolutions vertically.
  ## Args:
  ##   solutions: a non-empty sequence of LytSolution objects
  ## Returns:
  ##   A LytSolution object that lays out the solutions vertically, separated by
  ##   newlines, with the same left margin.


  assert solutions.len > 0

  if len(solutions) == 1:
    return solutions[0]

  var
    solutions = solutions # XXXX
    col: LytSolutionFactory

  for s in mitems(solutions):
    s.reset()

  var margin = 0  # Margin for all components
  while true:
    col.add(
      margin,
      solutions[^1].curSpan(),
      solutions.mapIt(it.curValueAt(margin)).sum(),
      solutions.mapIt(it.curGradient()).sum(),
      solutions.mapIt(it.curLayout()).getStacked()
    )

    # The distance to the closest next knot from the current margin.
    let d_star = min(
      solutions.
      filterIt(it.nextKnot() > margin).
      mapIt(it.nextKnot() - margin))  # TODO(pyelland): Redundant check?

    if d_star.inf:
      break

    margin += d_star

    for s in mitems(solutions):
      s.moveToMargin(margin)

  result = col.makeSolution()

proc hPlusSolution(s1, s2: var LytSolution, opts: LytOptions): LytSolution =
  ## The LytSolution that results from joining two LytSolutions side-by-side.

  ## Args:
  ##   `s1`: LytSolution object
  ##   `s2`: LytSolution object
  ## Returns:
  ##   A new LytSolution reflecting a layout in which `s2` ('s layout) is
  ##   placed immediately to the right of `s1`.

  ## The resulting LytSolution object maps each prospective left margin m
  ## to the span, cost and layout information that would result from
  ## siting LytSolution `s1` at m, and then placing `s2` at margin `m +
  ## sp1(m)`, where `sp1(m)` is the span of characters occupied by the
  ## layout to which `s1` maps m. In general, of course, both s1 and
  ## `s2`'s layouts may occupy multiple lines, in which case `s2`'s
  ## layout begins at the end of the last line of `s1`'s layout---the
  ## span in this case is the span of `s1`'s last line.
  var col: LytSolutionFactory
  s1.reset()
  s2.reset()
  var
    s1_margin = 0
    s2_margin = s1.curSpan()

  s2.moveToMargin(s2_margin)

  while true:
    # When forming the composite cost gradient and intercept, we must
    # eliminate the over-counting of the last line of the s1, which is
    # attributable to its projection beyond the margins.
    let
      g1 = s1.curGradient()
      g2 = s2.curGradient()
      overhang0 = s2_margin - opts.leftMargin  # s2_margin = rightMargin + span of s1
      overhang1 = s2_margin - opts.rightMargin  # s2_margin = rightMargin + span of s1
      g_cur = (g1 + g2 -
               opts.leftMarginCost * (overhang0 >= 0) -
               opts.rightMarginCost * (overhang1 >= 0))
      i_cur = (s1.curValueAt(s1_margin) + s2.curValueAt(s2_margin) -
               opts.leftMarginCost * max(overhang0, 0) -
               opts.rightMarginCost * max(overhang1, 0))

    # The Layout computed by the following implicitly sets the margin
    # for s2 at the end of the last line printed for s1.
    col.add(
      s1_margin, s1.curSpan() + s2.curSpan(), i_cur, g_cur,
      initLayout(@[
        lytPrint(s1.curLayout()),
        lytPrint(s2.curLayout())
    ]))

    # Move to the knot closest to the margin of the corresponding
    # component.
    let
      kn1 = s1.nextKnot()
      kn2 = s2.nextKnot()

    if kn1.inf and kn2.inf:
      break

    # Note in the following that one of kn1 or kn2 may be infinite.
    if kn1 - s1_margin <= kn2 - s2_margin:
      s1.advance()
      s1_margin = kn1
      s2_margin = s1_margin + s1.curSpan()
      # Note that s1.CurSpan() may have changed, and s2_margin may
      # decrease, so we cannot simply increment s2's index.
      s2.moveToMargin(s2_margin)
    else:
      s2.advance()
      s2_margin = kn2
      s1_margin = s2_margin - s1.curSpan()

  return col.makeSolution()




func plusConst(self: LytSolution, val: float): LytSolution =
  ## Add a constant to all values of this LytSolution.
  result = self
  for a in mitems(result.intercepts):
    a += val

proc withRestOfLine(
    self: var Option[LytSolution],
    rest: var Option[LytSolution], opts: LytOptions
  ): Option[LytSolution] =
  ## Return a LytSolution that joins the rest of the line right of this one.

  ## Args:
  ##   rest: a LytSolution object representing the code laid out on the
  ##     remainder of the line, or None, if the rest of the line is empty.
  ## Returns:
  ##   A new LytSolution object juxtaposing the layout represented by this
  ##   LytSolution to the immediate right of the remainder of the line.
  if rest.isNone():
    self
  else:
    some self.get().hplusSolution(rest.get(), opts)


#*************************************************************************#
#*****************************  LytBlock type  ******************************#
#*************************************************************************#
proc elements(self: LytBlock): seq[LytBlock] =
  if contains({bkWrap}, self.kind):
    return self.elements
  if contains({bkStack, bkChoice, bkLine}, self.kind):
    return self.elements
  raiseAssert("#[ IMPLEMENT:ERRMSG ]#")

proc `elements=`(self: var LytBlock; it: seq[LytBlock]) =
  var matched: bool = false
  if contains({bkWrap}, self.kind):
    if true:
      matched = true
      self.wrapElements = it
  if contains({bkStack, bkChoice, bkLine}, self.kind):
    if true:
      matched = true
      self.elements = it
  if not matched:
    raiseAssert("#[ IMPLEMENT:ERRMSG ]#")

func len*(blc: LytBlock): int =
  case blc.kind:
    of bkWrap:
      blc.wrapElements.len()

    of bkStack, bkChoice, bkLine:
      blc.elements.len()

    else:
      0


func textLen*(b: LytBlock): int =
  b.minWidth

func `[]`*(blc: LytBlock, idx: int): LytBlock =
  blc.elements[idx]

func `[]`*(blc: var LytBlock, idx: int): var LytBlock =
  blc.elements[idx]

iterator items*(blc: LytBlock): LytBlock =
  for item in blc.elements:
    yield item

iterator pairs*(blc: LytBlock): (int, LytBlock) =
  for idx, item in blc.elements:
    yield (idx, item)

iterator mitems*(blc: var LytBlock): var LytBlock =
  for item in mitems(blc.elements):
    yield item

iterator mpairs*(blc: var LytBlock): (int, var LytBlock) =
  for idx, item in mpairs(blc.elements):
    yield (idx, item)

#============================  Constructors  =============================#

func getBId(): int =
  var id {.global.}: int
  {.cast(noSideEffect).}:
    inc id
    return id

func makeBlock*(kind: LytBlockKind, breakMult: int = 1): LytBlock =
  result = LytBlock(
    id: getBId(),
    kind: kind, breakMult: breakMult, minWidth: 0, isBreaking: false)

  if kind == bkVerb:
    result.isBreaking = true

func makeEmptyBlock*(): LytBlock =
  LytBlock(
    id: getBId(), kind: bkEmpty, breakMult: 1, minWidth: 0, isBreaking: false)

func filterEmpty*(blocks: openarray[LytBlock]): seq[LytBlock] =
  for bl in blocks:
    if bl.kind != bkEmpty:
      result.add bl

func makeTextBlock*(
    text: string,
    breakMult: int = 1,
    breaking: bool = false
  ): LytBlock =

  assert not breaking
  result = LytBlock(
    kind: bkText, text: lytStr(text), isBreaking: breaking,
    id: getBId(), breakMult: breakMult, minWidth: 0
  )

  result.minWidth = result.text.len()

proc makeTextBlocks*(text: openarray[string]): seq[LytBlock] =
  text.mapIt(makeTextBlock(it))


func makeIndentBlock*(
  blc: LytBlock, indent: int, breakMult: int = 1): LytBlock


func isEmpty*(bl: LytBlock): bool {.inline.} =
  bl.kind == bkEmpty or
  (bl.kind in {bkStack, bkLine, bkChoice} and bl.len == 0)

template findSingle*(elems: typed, targetKind: typed): untyped =
  var
    countEmpty = 0
    countFull = 0
    idx = -1

  for item in elems:
    if item.isEmpty():
      inc countEmpty

    elif (
      when targetKind is set:
        item.kind in targetKind
      else:
        item.kind == targetKind
    ):
      if idx != -1:
        idx = -1
        break

      else:
        idx = countFull

    inc countFull

  if countFull == countEmpty + 1 and idx != -1:
    idx

  else:
    -1


func updateSizes(bk: var LytBlock) =
  bk.minWidth =
    case bk.kind:
      of bkStack: bk.elements.mapIt(it.minWidth).max()
      of bkLine: bk.elements.mapIt(it.minWidth).sum()
      of bkText: bk.text.len()
      of bkChoice: bk.elements.mapIt(it.minWidth).min()
      of bkVerb: bk.textLines.mapIt(it.len).max()
      else: 0


  bk.hasInnerChoice =
    case bk.kind:
      of bkStack, bkLine: bk.elements.anyIt(it.hasInnerChoice)
      of bkChoice: true
      else: false

  if bk.kind in { bkChoice , bkLine, bkStack } and bk.elements.len > 0:
    bk.isBreaking = bk.elements[^1].isBreaking


func convertBlock*(bk: LytBlock, newKind: LytBlockKind): LytBlock =
  result = LytBlock(
    id: getBId(), breakMult: bk.breakMult, kind: newKind,
    minWidth: 0, isBreaking: false
  )

  result.elements = bk.elements

  updateSizes(result)


func flatten*(bl: LytBlock, kind: set[LytBlockKind]): LytBlock =
  if bl.kind in kind and
     (let idx = findSingle(bl.elements, {
    low(LytBlockKind) .. high(LytBlockKind) } - { bkEmpty }); idx != -1):
    result = bl.elements[idx]

  else:
    result = bl

func makeChoiceBlock*(
    elems: openarray[LytBlock],
    breakMult: int = 1
  ): LytBlock =

  result = LytBlock(
    id: getBId(),
    isBreaking: false,
    breakMult: breakMult,
    kind: bkChoice,
    minWidth: 0,
    elements: filterEmpty(elems))

  updateSizes(result)


func makeLineBlock*(
    elems: openarray[LytBlock],
    breakMult: int = 1
  ): LytBlock =

  result = LytBlock(
    id: getBId(), isBreaking: false,
    breakMult: breakMult, kind: bkLine,
    minWidth: 0, elements: filterEmpty(elems))

  updateSizes(result)

func makeIndentBlock*(
    blc: LytBlock, indent: int, breakMult: int = 1): LytBlock =

  if indent == 0:
    blc

  else:
    makeLineBlock(@[makeTextBlock(" ".repeat(indent)), blc])



proc makeStackBlock*(elems: openarray[LytBlock], breakMult: int = 1): LytBlock =
  result = LytBlock(
    id: getBId(), isBreaking: false,
    minWidth: 0,
    breakMult: breakMult, kind: bkStack,
    elements: filterEmpty(elems))

  updateSizes(result)


func makeWrapBlock*(
    elems: openarray[LytBlock],
    breakMult: int = 1,
    sep: string = ", "
  ): LytBlock =

  LytBlock(
    isBreaking: false,
    id: getBId(),
    sep: sep,
    kind: bkWrap,
    wrapElements: toSeq(elems),
    breakMult: breakMult,
    minWidth: elems.mapIt(it.minWidth).max())

func makeVerbBlock*(
    textLines: openarray[string],
    breaking: bool = true,
    firstNl: bool = false,
    breakMult: int = 1
  ): LytBlock =

  assert breaking
  result = LytBlock(
    breakMult: breakMult,
    id: getBId(), kind: bkVerb,
    textLines: mapIt(textLines, lytStr(it)),
    isBreaking: breaking,
    firstNl: firstNl,
    minWidth: textLines.mapIt(it.len).max()
  )

  updateSizes(result)


func makeTextOrStackTextBlock*(
    text: string,
    breaking: bool = false,
    firstNl: bool = false,
    breakMult: int = 1
  ): LytBlock =

  if '\n' in text:
    let ls = text.splitLines(keepEol = false)
    result = makeStackBlock(
      mapIt(ls, makeTextBlock(it)),
      breakMult)

  else:
    result = makeTextBlock(text, breakMult)


func makeForceLinebreak*(text: string = ""): LytBlock =
  makeVerbBlock(@[text], true, false)

func makeLineCommentBlock*(
  text: string, prefix: string = "# "): LytBlock =
  makeVerbBlock(@[prefix & text])

func add*(target: var LytBlock, other: varargs[LytBlock]) =
  for bl in other:
    if bl.kind != bkEmpty:
      target.elements.add bl

  updateSizes(target)

proc makeAlignedGrid*(
    blocks: seq[seq[LytBlock]],
    aligns: openarray[tuple[
      leftPad, rightPad: int, direction: StringAlignDirection]]
  ): LytBlock =

  for idx, row in pairs(blocks):
    assert len(aligns) >= len(row):
      "Invalid number for column alignments specified - row " &
      $idx & " has total of " & $len(row) & " cells, but only " &
      $len(aligns) & " were specified."

  var colWidths = newSeqWith(len(aligns), 0)

  for rowIdx, row in pairs(blocks):
    for colIdx, col in pairs(row):
      colWidths[colIdx] = max(textLen(col), colWidths[colIdx])

  result = makeStackBlock([])
  for row in items(blocks):
    var resRow = makeLineBlock([])
    for idx, col in pairs(row):
      let al = aligns[idx]
      let diff = colWidths[idx] - textLen(col)
      case al.direction:
        of sadLeft:
          resRow.add makeLineBlock([
            makeTextBlock(repeat(" ", al.leftPad)),
            col,
            makeTextBlock(repeat(" ", al.rightPad + diff))])

        of sadRight:
          resRow.add makeLineBlock([
            makeTextBlock(repeat(" ", al.leftPad + diff)),
            col,
            makeTextBlock(repeat(" ", al.rightPad))])

        of sadCenter:
          let left = diff div 2
          let right = diff - left
          resRow.add makeLineBlock([
            makeTextBlock(repeat(" ", al.leftPad + left)),
            col,
            makeTextBlock(repeat(" ", al.rightPad + right))])



    result.add resRow


proc makeAlignedGrid*(
    blocks: seq[seq[LytBlock]],
    aligns: openarray[StringAlignDirection]
  ): LytBlock =

  makeAlignedGrid(blocks, mapIt(aligns, (0, 0, it)))


#============================  Layout logic  =============================#

proc doOptLayout*(
  self: var LytBlock,
  rest: var Option[LytSolution], opts: LytOptions): Option[LytSolution]

proc optLayout(
    self: var LytBlock,
    rest: var Option[LytSolution],
    opts: LytOptions
  ): Option[LytSolution] =
  ## Retrieve or compute the least-cost (optimum) layout for this block.
  ## - @arg{rest} :: text to the right of this block.
  ## - @ret{} :: Optimal layout for this block and the rest of the line.
  # Deeply-nested choice block may result in the same continuation
  # supplied repeatedly to the same block. Without memoisation, this
  # may result in an exponential blow-up in the layout algorithm.
  if rest notin self.layoutCache:
    self.layoutCache[rest] = self.doOptLayout(rest, opts)

  return self.layoutCache[rest]

proc doOptTextLayout(
  self: LytBlock,
  rest: var Option[LytSolution], opts: LytOptions): Option[LytSolution] =

  let
    span = len(self.text)
    layout = initLayout(@[lytString(self.text)])
  # The costs associated with the layout of this block may require 1, 2 or 3
  # knots, depending on how the length of the text compares with the two
  # margins (leftMargin and rightMargin) in opts. Note that we assume
  # opts.rightMargin >= opts.leftMargin >= 0, as asserted in base.Options.Check().
  if span >= opts.rightMargin:
    result = some initSolution(
      @[0],
      @[span],
      @[float(
        (span - opts.leftMargin) * opts.leftMarginCost +
        (span - opts.rightMargin) * opts.rightMargin)],
      @[float(opts.leftMarginCost + opts.rightMarginCost)],
      @[layout]
    )

  elif span >= opts.leftMargin:
    result = some initSolution(
      @[0, opts.rightMargin - span],
      @[span, span], # XXXX
      @[float((span - opts.leftMargin) * opts.leftMarginCost),
        float((opts.rightMargin - opts.leftMargin) * opts.leftMarginCost)],
      @[float(opts.leftMarginCost), float(opts.leftMarginCost + opts.rightMarginCost)],
      @[layout, layout] # XXXX
    )
  else:
    result = some initSolution(
      @[0, opts.leftMargin - span, opts.rightMargin - span],
      @[span, span, span], # XXXX
      @[float(0), float(0), float((opts.rightMargin - opts.leftMargin) * opts.leftMarginCost)],
      @[float(0), float(opts.leftMarginCost), float(opts.leftMarginCost + opts.rightMarginCost)],
      @[layout, layout, layout] # XXXX
    )

  return result.withRestOfLine(rest, opts)


proc doOptLineLayout(
    self: var LytBlock,
    rest: var Option[LytSolution],
    opts: LytOptions
  ): Option[LytSolution] =

  assert self != nil
  if self.elements.len == 0:
    return rest

  var elementLines: seq[seq[LytBlock]] = @[]
  elementLines.add @[]

  for i, elt in self.elements:
    elementLines[^1].add elt
    if i < self.elements.high() and elt.isBreaking:
      elementLines.add @[]

  if len(elementLines) > 1:
    assert opts.format_policy.breakElementLines != nil
    elementLines = opts.format_policy.breakElementLines(elementLines)

  var lineSolns: seq[LytSolution]

  for i, ln in mpairs(elementLines):
    var lnLayout =
      if i == elementLines.high:
        rest
      else:
        none(LytSolution)

    for idx, elt in rmpairs(ln):
      lnLayout = elt.optLayout(lnLayout, opts)

    if lnLayout.isSome():
      lineSolns.add lnLayout.get()

  let soln = vSumSolution(lineSolns)

  result = some soln.plusConst(
    float(opts.linebreakCost * (len(lineSolns) - 1)))


proc doOptChoiceLayout(
  self: var LytBlock, rest: var Option[LytSolution],
  opts: LytOptions): Option[LytSolution] =
  # The optimum layout of this block is simply the piecewise minimum of its
  # elements' layouts.
  return minSolution():
    var tmp: seq[LytSolution]
    for it in mitems(self.elements):
      let lyt = it.optLayout(rest, opts)
      if lyt.isSome():
        tmp.add lyt.get()

    tmp


proc doOptStackLayout(
    self: var LytBlock,
    rest: var Option[LytSolution],
    opts: LytOptions
  ): Option[LytSolution] =

  # The optimum layout for this block arranges the elements vertically. Only
  # the final element is composed with the continuation provided---all the
  # others see an empty continuation ("None"), since they face the end of
  # a line.
  if self.elements.len == 0:
    return rest

  let soln = vSumSolution: get: collect(newSeq):
    for idx, elem in mpairs(self.elements):
      if idx < self.elements.high:
        var it = none(LytSolution)
        optLayout(elem, it, opts)
      else:
        elem.optLayout(rest, opts)


  # Under some odd circumstances involving comments, we may have a
  # degenerate solution.
  # WARNING
  if soln.layouts.len == 0:
    return rest

  # Add the cost of the line breaks between the elements.
  return some soln.plusConst float(
    opts.linebreakCost * self.break_mult *
    max(len(self.elements) - 1, 0))


proc doOptWrapLayout(
  self: var LytBlock,
  rest: var Option[LytSolution], opts: LytOptions): Option[LytSolution] =
  ## Computing the optimum layout for this class of block involves
  ## finding the optimal packing of elements into lines, a problem
  ## which we address using dynamic programming.
  var sep_layout = block:
    var it = (makeTextBlock(self.sep), none(LytSolution))
    it[0].optLayout(it[1], opts)

  # TODO(pyelland): Investigate why OptLayout doesn't work here.
  var prefix_layout: Option[LytSolution] =
    if self.prefix.isSome():
      var it = (makeTextBlock(self.prefix.get()), none(LytSolution))
      it[0].doOptLayout(it[1], opts)
    else:
      none(LytSolution)

  var elt_layouts = block:
    var res: seq[Option[LytSolution]]
    for it in mitems(self.wrapElements):
      var tmp = none(LytSolution)
      res.add it.optLayout(tmp, opts)

    res

  # Entry i in the list wrap_solutions contains the optimum layout for the
  # last n - i elements of the block.
  var wrap_solutions: seq[Option[LytSolution]] =
    self.len.newSeqWith(none(LytSolution))

  # Note that we compute the entries for wrap_solutions in reverse
  # order, at each iteration considering all the elements from i ... n
  # - 1 (the actual number of elements considered increases by one on
  # each iteration). This means that the complete solution, with
  # elements 0 ... n - 1 is computed last.
  for i in countdown(self.len - 1, 0): # XXXX
    # To calculate wrap_solutions[i], consider breaking the last n - i
    # elements after element j, for j = i ... n - 1. By induction,
    # wrap_solutions contains the optimum layout of the elements after
    # the break, so the full layout is calculated by composing a line
    # with the elements before the break with the entry from
    # wrap_solutions corresponding to the elements after the break.
    # The optimum layout to be entered into wrap_solutions[i] is then
    # simply the minimum of the full layouts calculated for each j.
    var solutions_i: seq[LytSolution]
    # The layout of the elements before the break is built up incrementally
    # in line_layout.
    var line_layout: Option[LytSolution] =
      if prefix_layout.isNone():
        elt_layouts[i]
      else:
        prefix_layout.withRestOfLine(elt_layouts[i], opts)

    var last_breaking: bool = self.wrapElements[i].isBreaking
    for j in i ..< self.len - 1: # XXXX
      let full_soln = vSumSolution(
        @[line_layout, wrap_solutions[j + 1]].get())
      # We adjust the cost of the full solution by adding the cost of
      # the line break we've introduced, and a small penalty
      # (_options.cpack) to favor (ceteris paribus) layouts with
      # elements packed into earlier lines.
      solutions_i.add(full_soln.plusConst float(
        opts.linebreakCost * self.break_mult + opts.cpack * (self.len - j)))
      # If the element at the end of the line mandates a following
      # line break, we're done.
      if last_breaking:
        break
      # Otherwise, add a separator and the next element to the line
      # layout and continue.
      var sep_elt_layout = sep_layout.withRestOfLine(
        elt_layouts[j + 1], opts)

      line_layout = line_layout.withRestOfLine(sep_elt_layout, opts)
      last_breaking = self.wrapElements[j + 1].isBreaking

    if not last_breaking:
      solutions_i.add line_layout.withRestOfLine(rest, opts).get()

    wrap_solutions[i] = minSolution(solutions_i)
  # Once wrap_solutions is complete, the optimum layout for the entire
  # block is the optimum layout for the last n - 0 elements.
  return wrap_solutions[0]

proc doOptVerbLayout(
    self: var LytBlock,
    rest: var Option[LytSolution],
    opts: LytOptions
  ): Option[LytSolution] =


  # The solution for this block is essentially that of a TextBlock(''), with
  # an abberant layout calculated as follows.
  var lElts: seq[LayoutElement]

  for i, ln in self.textLines:
    if i > 0 or self.first_nl:
      lElts.add lytNewLine()

    lElts.add lytString(ln)

  let layout = initLayout(lElts)
  let span = 0
  var sf: LytSolutionFactory
  if opts.leftMargin > 0:  # Prevent incoherent solutions
    sf.add(0, span, 0, 0, layout)
  # opts.rightMargin == 0 is absurd
  sf.add(opts.leftMargin - span, span, 0, opts.leftMarginCost, layout)
  sf.add(
    opts.rightMargin - span, span,
    (opts.rightMargin - opts.leftMargin) * opts.leftMarginCost,
    opts.leftMarginCost + opts.rightMarginCost, layout)

  result = some sf.makeSolution()

proc doOptLayout*(
    self: var LytBlock,
    rest: var Option[LytSolution],
    opts: LytOptions
  ): Option[LytSolution] =

  case self.kind:
    of bkText:   result = self.doOptTextLayout(rest, opts)
    of bkLine:   result = self.doOptLineLayout(rest, opts)
    of bkChoice: result = self.doOptChoiceLayout(rest, opts)
    of bkStack:  result = self.doOptStackLayout(rest, opts)
    of bkWrap:   result = self.doOptWrapLayout(rest, opts)
    of bkVerb:   result = self.doOptVerbLayout(rest, opts)
    of bkEmpty:  assert false


const defaultFormatOpts* = LytOptions(
  leftMargin: 0,
  rightMargin: 80,
  leftMarginCost: 0.05,
  rightMarginCost: 100,
  linebreakCost: 5,
  indentSpaces: 2,
  cpack: 0.001,
  formatPolicy: LytFormatPolicy(
    breakElementLines: (
      proc(blc: seq[seq[LytBlock]]): seq[seq[LytBlock]] =

        let spaceText = makeTextBlock(" ")
        func strippedLine(line: seq[LytBlock]): LytBlock =
          return makeLineBlock(line#[(leftSpaces)..(rightSpaces)]#)

        result.add @[blc[0]]
        if blc.len > 1:
          let ind = makeIndentBlock(
            makeStackBlock(blc[1..^1].map(strippedLine)),
            2 * 2)

          result.add @[ind])))

type
  LytBuilderKind* = enum
    blkLine
    blkStack
    blkText
    blkIndent
    blkSpace
    blkChoice
    blkEmpty
    blkWrap

proc `[]`*(
    b: static[LytBuilderKind], s: seq[LytBlock], sep: string = ", "
  ): LytBlock =
  static:
    assert b in {blkLine, blkStack, blkChoice, blkWrap},
      "Layout builder for block sequences must be a line, start, choice " &
      "or wrap, but found " & $b &
      "Change builder kind, or add missing arguments"

  case b:
    of blkLine:
      makeLineBlock(s)

    of blkStack:
      makeStackBlock(s)

    of blkChoice:
      makeChoiceBlock(s)

    of blkWrap:
      makeWrapBlock(s, sep = sep)

    else:
      raiseAssert("#[ IMPLEMENT ]#")


proc `[]`*(
    b: static[LytBuilderKind],
    bl: LytBlock,
    args: varargs[LytBlock],
  ): LytBlock =
  static:
    assert b in {blkLine, blkStack, blkChoice, blkWrap},
      "Layout builder for block sequences must be a line, start, choice " &
      "or wrap, but found " & $b &
      "Change builder kind, or add missing arguments"

  b[@[ bl ] & toSeq(args)]

proc `[]`*(
    b: static[LytBuilderKind],
    a: string,
    breaking: bool = false
  ): LytBlock =

  static:
    assert(
      b == blkText,
      "Single-argument block builder for string must use `T[\"somestring\"]`" &
      "Change builder kind to `T` (current kind is " & $b & ")"
    )

  return makeTextOrStackTextBlock(a, breaking)

proc `[]`*(b: static[LytBuilderKind], tlen: int = 1): LytBlock =
  case b:
    of blkSpace: result = makeTextBlock(" ".repeat(tlen))
    of blkEmpty: result = makeEmptyBlock()
    of blkLine: result = makeLineBlock(@[])
    of blkChoice: result = makeChoiceBlock(@[])
    of blkStack: result = makeStackBlock(@[])
    of blkWrap: result = makeWrapBlock(@[])
    else:
      static:
        assert(
          b in {blkSpace, blkLine, blkChoice, blkStack, blkEmpty, blkWrap},
          "Block builder without arguments must use space or " &
          "combinator layouts. " & "Unexpected builder kind"
        )




proc `[]`*(b: static[LytBuilderKind], i: int, bl: LytBlock): LytBlock =
  static: assert b == blkIndent
  return makeIndentBlock(bl, i)

func `&?`*(bl: LytBlock, added: tuple[condOk: bool, bl: LytBlock]): LytBlock =
  result = bl
  if added.condOk:
    result.add added.bl

func `??`*(bl: LytBlock, condOk: bool): LytBlock =
  if condOk: bl else: makeEmptyBlock()

func `??`*(blocks: tuple[ok, fail: LytBlock], condOk: bool): LytBlock =
  if condOk: blocks.ok else: blocks.fail

func condOr*(
    cond: bool,
    ok: LytBlock,
    fail: LytBlock = makeEmptyBlock()): LytBlock =
  if cond: ok else: fail



func join*(
    blocks: LytBlock,
    sep: LytBlock,
    vertLines: bool = true
  ): LytBlock =
  assert blocks.kind in {bkLine, bkStack},
    "Only stack or line layouts can be joined"

  result = makeBlock(blocks.kind)

  for idx, item in pairs(blocks):
    let isLast = idx == len(blocks) - 1
    if blocks.kind == bkStack and vertLines:
      if not isLast:
        result.add makeLineBlock([item, sep])

      else:
        result.add item

    else:
      result.add item
      if not isLast:
        result.add sep

func join*(
    blocks: seq[LytBlock],
    sep: LytBlock,
    direction: LytBlockKind
  ): LytBlock  =

  result = makeBlock(direction)
  for idx, item in pairs(blocks):
    let isLast = idx == len(blocks) - 1
    result.add item
    if not isLast:
      result.add sep

template addItBlock*(
    res: LytBlock,
    item: typed,
    expr: untyped,
    join: LytBlock
  ): untyped =

  var idx = 0
  for idx, it {.inject.} in pairs(item):
    if idx < item.high:
      if res.kind == bkStack:
        res.add makeLineBlock(@[expr, join])

      else:
        res.add expr
        res.add join

    else:
      res.add expr


template joinItBlock*(
    direction: LytBlockKind,
    item: typed,
    expr: untyped,
    join: LytBlock
  ): untyped =

  var res = makeBlock(direction)
  res.addItBlock(item, expr, join)
  res

template joinItLine*(
    item: typed,
    expr: untyped,
    join: LytBlock
  ): untyped =

  var res = makeBlock(bkLine)
  res.addItBlock(item, expr, join)
  res

proc toLayouts*(
    bl: LytBlock, opts: LytOptions = defaultFormatOpts): seq[Layout] =

  assert(not(
     (bl.kind in {bkStack, bkLine, bkChoice} and bl.elements.len == 0) or
     (bl.kind in {bkWrap} and bl.wrapElements.len == 0)),
        "Invalid combinator layout block passed - no nested elements " &
        "specified, so layout is impossible. Block kind - " &
        $bl.kind)

  case bl.kind:
    of bkStack, bkChoice, bkLine:
      assert 0 < bl.elements.len

    of bkWrap:
      assert 0 < bl.wrapElements.len

    else:
      discard

  var bl = bl
  let sln = block:
    var it = none(LytSolution)
    bl.doOptLayout(it, opts)

  assert isSome(sln), "Could not perform layout for block " & $bl

  return sln.get().layouts

proc toString*(
    bl: LytBlock,
    rightMargin: int = 80,
    opts: LytOptions = defaultFormatOpts
  ): string =

  if bl.kind == bkEmpty:
    return ""

  else:
    var bl = bl
    let opts = block:
      var it = opts
      it.rightMargin = rightMargin
      it

    var console: OutConsole
    let lyt = bl.toLayouts()[0]
    lyt.printOn(console)
    return console.outStr



func codegenRepr*(inBl: LytBlock, indent: int = 0): string =
  func aux(bl: LytBlock, level: int): string =
    let pref = repeat("  ", level)
    let name =
      case bl.kind:
        of bkEmpty: "E"
        of bkLine: "H"
        of bkChoice: "C"
        of bkText: "T"
        of bkWrap: "W"
        of bkStack: "V"
        of bkVerb: "T"

    case bl.kind:
      of bkLine, bkChoice, bkStack, bkWrap:
        result = pref & name & "[\n"
        for idx, elem in pairs(bl.elements):
          let isLast = idx == len(bl.elements) - 1
          result &= elem.aux(level + 1) & (if isLast: "]" else: ",\n")

        # result &= pref & "]"

      of bkText:
        let text = bl.text.text.mapIt($it).join("").escapeStrLit()
        result = &"{pref}T[\"{text}\"]"

      of bkVerb:
        result = pref & name & "["
        for idx, line in pairs(bl.textLines):
          if idx == 0: result &= "\""
          result &= escapeStrLit($line)
          if idx == len(bl.textLines) - 1: result &= "\"" else: result &= "\\n"

        result &= "]"

      of bkEmpty:
        result = "E[]"

  return aux(inBl, indent)


func pyCodegenRepr*(
    inBl: LytBlock, indent: int = 0,
    nimpref: string = "",
    prelude: bool = false,
    colortext: bool = false,
    colored: bool = false,
    makeTextOrVerb: bool = false
  ): string =
  func aux(bl: LytBlock, level: int): string =
    let pref = repeat("  ", level)
    let name =
      case bl.kind:
        of bkEmpty: "E"
        of bkLine: nimpref & "LineBlock("
        of bkChoice: nimpref & "ChoiceBlock("
        of bkText: nimpref & "TB()"
        of bkWrap: nimpref & "W"
        of bkStack: nimpref & "StackBlock("
        of bkVerb: nimpref & "VerbBlock("

    case bl.kind:
      of bkLine, bkChoice, bkStack, bkWrap:
        result = pref & name & "[\n"
        for idx, elem in pairs(bl.elements):
          let isLast = idx == len(bl.elements) - 1
          result &= elem.aux(level + 1) & (if isLast: "])" else: ",\n")

      of bkText:
        let text = bl.text.text.escapeStrLit()

        result = &"{pref}{nimpref}TextBlock(\"{text}\")"

      of bkVerb:
        result = pref & name & "["
        for idx, line in pairs(bl.textLines):
          if idx == 0: result &= "\"" else: result &= "\", \""
          result &= escapeStrLit($line)
          if idx == len(bl.textLines) - 1: result &= "\""

        result &= "])"

      of bkEmpty:
        result = "E[]"

  if not prelude:
    result = aux(inBl, indent)

  else:
    result &= """
#!/usr/bin/env python2
from blocks import *
import base, re
import cStringIO

"""

    result &= "blc = " & aux(inBl, indent)
    result &= """

opts = base.Options()

opts.m0 = 0
opts.m1 = 50
opts.cpack = 1e-3

opts.c0 = 0.05
opts.c1 = 100
opts.cb = 2

lyt = blc.DoOptLayout(None)

outp = cStringIO.StringIO()
blc.PrintOn(outp)
print re.sub(r' *$', '', outp.getvalue(), flags=re.MULTILINE)
"""


template initBlockFmtDSL*() {.dirty.} =
  const
    H = blkLine
    V = blkStack
    T = blkText
    I = blkIndent
    S = blkSpace
    C = blkChoice
    E = blkEmpty
    W = blkWrap

when isMainModule:
  initBlockFmtDsl()

  if true:
    echo toString(
      H[
        T["proc ("],
        V[T["arg1: int"], T["arg2: int"], T["arg3: int"]].join(T[", "]),
        T[")"]
      ]
    )

  if true:
    echo toString(
      H[
        T["proc ("],
        C[
          V[@[T["arg1: int"], T["arg2: int"],]].join(T[", "])
        ],
        T[")"]
      ]
    )


  if true:
    echo toString(
      H[
        T["proc ("],
        C[
          H[@[T["arg1: int"], T["arg2: int"],]].join(T[", "]),
          V[@[T["arg1: int"], T["arg2: int"],]].join(T[", "]),
        ],
        T[")"]
      ],
      40
    )

  if true:
    for i in [1, 5, 10]:
      var blocks = mapIt(0 .. i, T[&"arg{it}: int{it}"])
      let bl = H[
        T["proc ("],
        C[join(H[blocks], T[", "]), join(V[blocks], T[","])],
        T[")"]
      ]

      # echo bl.treeRepr()

      echo toString(bl)

  echo toString(H[
    H[T["FnName"], T["("]],
    W[mapIt(1 .. 10, T[&"argument{it}"])],
    T[")"]
  ], 50)

  echo toString(H[
    H[T["FnName"], T["("]],
    W[mapIt(1 .. 10, T[&"argument{it}"])],
    T[")"]
  ], 30)

  echo toString(H[
    H[T["AVeryLongAndDescriptiveFunctionName"], T["("]],
    W[mapIt(1 .. 10, T[&"argument{it}"])],
    T[")"]
  ], 50)

  echo toString(C[
    H[
      H[T["AVeryLongAndDescriptiveFunctionName"], T["("]],
      W[mapIt(1 .. 10, T[&"argument{it}"])],
      T[")"]
    ],
    V[
      H[T["AVeryLongAndDescriptiveFunctionName"], T["("]],
      I[4, W[mapIt(1 .. 10, T[&"argument{it}"])]],
      T[")"]
    ]
  ], 50)
