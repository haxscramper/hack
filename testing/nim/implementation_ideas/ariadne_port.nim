import
  std/[
    options,
    strutils,
    strformat,
    sugar,
    sequtils
  ]


type
  Label[S] = object
    ## Single label for some span in the error message
    span*: S ## Unique identifier for the range in source code. Can be a
             ## simple `start..end` slice or a more complex type.
    # TEMP will be replaced by a colored string sequence, error messages
    # can have internal colorization
    msg*: Option[string] ## Error message to show in the output formatting
    color*: Option[int]
    order*: int ## Specify the order of this label relative to other labels.
    ##
    ## Lower values correspond to this label having an earlier order.
    ##
    ## If unspecified, labels default to an order of `0`.
    ##
    ## When labels are displayed after a line the crate needs to decide
    ## which labels should be displayed first. By Default, the orders
    ## labels based on where their associated line meets the text (see
    ## [`LabelAttach`]). Additionally, multi-line labels are ordered before
    ## inline labels. You can this this function to override this
    ## behaviour.

    priority*: int ## Specify the priority of this label relative to other
    ## labels.
    ##
    ## Higher values correspond to this label having a higher priority.
    ##
    ## If unspecified, labels default to a priority of `0`.
    ##
    ## Label spans can overlap. When this happens, the crate needs to
    ## decide which labels to prioritise for various purposes such as
    ## highlighting. By default, spans with a smaller length get a higher
    ## priority. You can this this function to override this behaviour.

func initLabel*[S](span: S): Label[S] =
  ## Create new error label with given span
  Label[S](span: span)

func setMessage*[S](label: var Label[S], msg: string) =
  label.msg = some msg

type
  LabelAttach = enum
    Start ## Arrows should attach to the start of the label span.
    Middle ## Arrows should attach to the middle of the label span (or as
           ## close to the middle as we can get).
    End ## Arrows should attach to the end of the label span.

  Config = object
    crossGap: bool ## When label lines cross one-another, should there be a
    ## gap?
    ##
    ## The alternative to this is to insert crossing characters. However,
    ## these interact poorly with label colours.
    labelAttach: LabelAttach ## Where should inline labels attach to their
                             ## spans?
    compact: bool ## Should the report remove gaps to minimise used space?
    underlines: bool ## Should underlines be used for label span where
                     ## possible?
    multilineArrows: bool ## Should arrows be used to point to the bounds
                          ## of multi-line spans?
    color: bool ## Should colored output should be enabled?
    tabWidth: int ## How many characters width should tab characters be?
    charSet: CharSet ## What character set should be used to display
                     ## dynamic elements such as boxes and arrows?

  CharSet = enum
    Unicode ## Unicode characters (an attempt is made to use only
            ## commonly-supported characters).
    Ascii ## ASCII-only characters.

  ReportKind = enum
    Error ## The report is an error and indicates a critical problem that
          ## prevents the program performing the requested action.
    Warning ## The report is a warning and indicates a likely problem, but
            ## not to the extent that the requested action cannot be
            ## performed.
    Advice ## The report is advice to the user about a potential
           ## anti-pattern of other benign issues.
    Custom ## The report is of a kind not built into Ariadne.

  Report[S] = object
    case kind*: ReportKind
      of Custom:
        customName: string
        customColor: int

      else:
        discard

    code: Option[string]
    msg: Option[string]
    note: Option[string]
    help: Option[string]
    location: (S, int)
    labels: seq[Label[S]]
    config: Config

type
  Characters = object
    hbar: char
    vbar: char
    xbar: char
    vbarBreak: char
    vbarGap: char

    uarrow: char
    rarrow: char

    ltop: char
    mtop: char
    rtop: char
    lbot: char
    rbot: char
    mbot: char

    lbox: char
    rbox: char

    lcross: char
    rcross: char

    underbar: char
    underline: char

func ascii(): Characters =
  Characters(
    hbar: '-',
    vbar: '|',
    xbar: '+',
    vbarBreak: '*',
    vbarGap: ':',
    uarrow: '^',
    rarrow: '>',
    ltop: ',',
    mtop: 'v',
    rtop: '.',
    lbot: '`',
    mbot: '^',
    rbot: '\'',
    lbox: '[',
    rbox: ']',
    lcross: '|',
    rcross: '|',
    underbar: '|',
    underline: '^',
  )

type
  LabelKind = enum
    Inline
    Multiline

  LabelInfo[S] = object
    kind: LabelKind
    label: Label[S]

  SourceGroup[S] = object
    srcId: S
    span: Slice[int]
    labels: seq[LabelInfo[S]]

  Line = object
    ## A type representing a single line of a souce
    offset: int
    len: int
    chars: string

  Source = object
    lines: seq[Line] ## A type representing a single source that may be
                     ## referred to by [`Span`]s.
    len: int ## In most cases, a source is a single input file.

  Cache[Id] = object
    fetch: proc(self: var Cache[Id], id: Id)

proc `from`(str: string): Source =
  ## Generate a [`Source`] from the given [`str`].
  ##
  ## Note that this function can be expensive for long strings. Use an
  ## implementor of [`Cache`] where possible.
  var offset = 0
  for line in str.splitLines():
    result.lines.add Line(
      offset: offset,
      len: line.len() + 1,
      chars: line.strip()
    )

    offset += line.len

  result.len = offset


iterator chars(self: Source): char =
  ## Iterate over all characters in the source
  for line in self.lines:
    for ch in line.chars:
      yield ch

proc line(self: Source, index: int): Line =
  ## Get access to a specific, zero-indexed [`Line`].
  return self.lines[index]

proc saturatingSub(i: int, diff: int): int =
  if i < diff:
    return 0

  else:
    return i - diff

  
proc getOffsetLine(self: Source, offset: int): Option[(Line, int, int)] =
  if offset <= self.len:
    for idx, line in self.lines:
      if line.offset == offset:
        let
          idx = idx.saturatingSub(1)
          line = self.lines[idx]

        assert(
          offset >= line.offset,
          &"offset = {offset}, line.offset = {line.offset}")
        return some((line, idx, offset - line.offset))



proc start() = discard
proc finish() = discard

proc getLineRange[S](self: Source, span: S): Slice[int] =
  ## Get the range of lines that this span runs across.
  ##
  ## The resulting range is guaranteed to contain valid line indices (i.e:
  ## those that can be used for [`Source::line`]).

  let start = self.getOffsetLine(start(span)).mapOr(0, l => l[1])
  let finish = self.
    getOffsetLine(
      finish(span).
      saturatingSub(1).
      max(start(span))).
    mapOr(self.lines.len(), l => l[1] + 1)

  return start .. finish

func span(l: Line): Slice[int] = l.offset ..< l.offset + l.len



template findIt*(s: typed, op: untyped): int =
  ##[ Find first element of the sequence for which `op` evaluates as
  true and return it's index. If no such element is found return -1
  ]##

  var result = -1
  for idx, it {.inject.} in s:
    if op: result = idx; break

  result

proc getSourceGroups[S](
      self: Report[S], cache: var Cache[S]): seq[SourceGroup[S]] =

  for label in self.labels:
    let srcDisplay = cache.display(label.span.source())
    let tmp = cache.fetch(label.span.source())
    let src =
      if tmp.isOk():
        tmp.get()
      else:
        echo("Unable to fetch source '{srcDisplay}'")
        continue

    let startLine = src.getOffsetLine(label.span.start()).mapIt(it[1])
    let finishLine = src.getOffsetLine(
      label.span.finish().saturatingSub(1).max(
        label.span.start())).mapIt(it[1])

    let labelInfo = LabelInfo(
      kind: if startLine == finishLine: Inline else: Multiline,
      label: label
    )


    let idx = result.findIt(it.srcId == label.span.source())
    if idx != -1:
      result[idx].span.start = result[idx].span.start.min(label.span.start())
      result[idx].span.finish = result[idx].span.finish.max(
        label.span.finish())
      result[idx].labels.push(labelInfo)

    else:
      result.add(SourceGroup(
        srcId: label.span.source(),
        span: start(label.span) .. finish(label.span),
        labels: @[labelInfo],
      ));

import std/streams

proc write[S, Id](self: Report[S], cache: var Cache[Id], w: Stream) =
  let draw = case self.config.charSet:
    of CharSet.Unicode: Characters.unicode()
    of CharSet.Ascii: Characters.ascii()

  # --- Header ---

  let
    code = self.code.asRef().mapIt(&"[{it}] ")
    id = &"{code}{self.kind}:"
    kindColor = case self.kind:
      of ReportKind.Error: self.config.errorColor()
      of ReportKind.Warning: self.config.warningColor()
      of ReportKind.Advice: self.config.adviceColor()
      of ReportKind.Custom: some(self.color)

  w.writeln(w, "{id.fg(kindColor)} {$self.msg.asRef()}")

  let groups = self.getSourceGroups(cache)

  # Line number maximum width
  var lineNoWidth: int = 0
  for group in groups:
    let srcName = cache
      .display(group.srcId)
      .map(it => it.toString())
      .get("<unknown>")

    let src = cache.fetch(group.srcId).get()
    let lineRange = src.getLineRange(span)

    # Some((1..)
    #     .map(|x| 10u32.pow(x))
    #     .takeWhile(|x| lineRange.end as u32 / x != 0)
    #     .count() + 1)

  # --- Source sections ---
  let groupsLen = groups.len()
  for groupIdx, group in groups:
    let
      srcId = group.srcId
      span = group.span
      labels = group.labels

    let srcName = cache
      .display(srcId)
      .map(it => it.toString())
      .get("<unknown>")

    let
      src = cache.fetch(srcId)
      lineRange = src.getLineRange(span)

    # File name & reference
    let location =
      if srcId == self.location:
        self.location
      else:
        labels[0].label.span.start()

    let (lineNo, colNo) = src
      .getOffsetLine(location)
      .mapIt((it[1], $(it[2] + 1)))
      .unwrapOrElse(|| ('?'.toString(), '?'.toString()))

    let lineRef = &":{lineNo}:{colNo}"
    w.writeLine(
      repeat(" ", lineNoWidth + 2),
      (if groupIdx == 0: draw.ltop else: draw.lcross)
        .fg(self.config.marginColor()),
      draw.hbar.fg(self.config.marginColor()),
      draw.lbox.fg(self.config.marginColor()),
      srcName,
      lineRef,
      draw.rbox.fg(self.config.marginColor()),
    )

    if not self.config.compact:
      writeLine(
        w,
        repeat(' ', lineNoWidth + 2),
        draw.vbar.fg(self.config.marginColor())
      )

    type
      LineLabel = object
        col: int
        label: Label[S]
        multi: bool
        drawMsg: bool

    # Generate a list of multi-line labels
    var multiLabels: seq[Label[S]]
    for labelInfo in labels:
      if labelInfo.kind == LabelKind.Multiline:
        multiLabels.add(labelInfo.label)

    # Sort multiline labels by length
    multiLabels.sortByIt(-it.span.len())

    proc writeMargin(
      idx: int,
      isLine: bool,
      isEllipsis: bool,
      drawLabels: bool,
      reportRow: Option[(int, bool)],
      lineLabels: seq[LineLabel[S]],
      marginLabel: Option[LineLabel[S]]
    ) =

      let lineNoMargin =
        if isLine and not isEllipsis:
          let lineNo = format("{}", idx + 1)
          concat(
            repeat((' ', lineNoWidth - lineNo.chars().count())),
            lineNo,
            draw.vbar,
          )
        else:
          concat(
            repeat((' ', lineNoWidth + 1)),
            if isEllipsis: draw.vbarGap else: draw.vbarBreak
          )

      write(
        w,
        lineNoMargin.fg(self.config.marginColor()),
        repeat(some(' ').filterIt(not self.config.compact)),
      )

      # Multi-line margins
      if drawLabels:
        for col in 0 ..< multiLabels.len() + (multiLabels.len() > 0):
          var
            corner: Option[(Label[S], bool)]
            hbar: Option[Label[S]]
            vbar: Option[Label[S]]
            marginPtr: Option[(LineLabel[S], bool)]

          let
            multiLabel = multiLabels.get(col)
            lineSpan = src.line(idx).unwrap().span()

          for (i, label) in multiLabels[
            0 ..< (col + 1).min(multiLabels.len())
          ].iter().enumerate():

            let margin = marginLabel.filterIt(label == it.label)

            if label.span.start() <= lineSpan.end and
               label.span.end() > lineSpan.start:
              let
                isParent = i != col
                isStart = lineSpan.contains(label.span.start())
                isEnd = lineSpan.contains(label.lastOffset())

              if margin.filterOpt(it => it.isLine).canGet(margin):
                marginPtr = Some((margin, isStart))

              elif not isStart and (not isEnd or isLine):
                vbar = vbar.or(Some(*label).filter(|_| not isParent))

              elif reportRow.canGet(reportRow):
                let (reportRow, isArrow) = reportRow
                let labelRow = lineLabels
                    .iter()
                    .enumerate()
                    .findItOpt(it[1] == label)
                    .map(it => it[1])
                    .get(0)

                if reportRow == labelRow:
                  if margin.canGet(margin):
                    vbar = Some(margin.label).filter(|_| col == i)
                    if isStart:
                      continue

                  if isArrow:
                    hbar = Some(**label)
                    if not isParent:
                      corner = Some((label, isStart))

                  elif not isStart:
                    vbar = vbar.or(Some(*label).filter(|_| not isParent))
                else:
                  vbar = vbar.or(Some(*label).filter(
                    |_| not isParent and (isStart ^
                      (reportRow < labelRow))))

          if let (Some((margin, IsStart)), true) = (marginPtr, isLine):
              let isCol = multiLabel.mapOr(false, |ml| **ml as *const _ == margin.label as *const _)
              let isLimit = col + 1 == multiLabels.len()
              if not isCol and not isLimit:
                  hbar = hbar.or(Some(margin.label))
              }
          }

          hbar = hbar.filter(|l| marginLabel.asRef().mapOr(true, |margin| margin.label as *const _ not = *l as *const _) or not isLine)

          let (a, b) = if let Some((label, isStart)) = corner:
            fg(if isStart: draw.ltop else: draw.lbot, label.color)
          elif let Some(label) = hbar.filter(|_| vbar.isSome() and not self.config.crossGap):
            (fg(label.color, draw.xbar), fg(label.color, draw.hbar))
          elif let Some(label) = hbar:
            (draw.hbar.fg(label.color), draw.hbar.fg(label.color))
          elif let Some(label) = vbar:
            (if isEllipsis: draw.vbarGap else: draw.vbar }.fg(label.color), ' '.fg(None))
          elif let (Some((margin, isStart)), true) = (marginPtr, isLine):
            let isCol = multiLabel.mapOr(false, |ml| **ml as *const _ == margin.label as *const _)
            let isLimit = col == multiLabels.len()
            (
              (if isLimit:
                  draw.rarrow
                else if isCol:
                  if isStart: draw.ltop else: draw.lcross
                else:
                  draw.hbar)
              .fg(margin.label.color),
              if not isLimit:
                    draw.hbar
              else:
                  ' '
              }.fg(margin.label.color),
            )
          else:
            (' '.fg(None), ' '.fg(None))

          write(w, "{}", a)
          if not self.config.compact:
            write(w, "{}", b)

    var isEllipsis = false
    for idx in lineRange:
      let line = if let Some(line) = src.line(idx):
        line
      else:
        continue

      let marginLabel = multiLabels
        .iter()
        .enumerate()
        .filterMap(|(I, label)|:
            let isStart = line.span().contains(label.span.start())
            let isEnd = line.span().contains(label.lastOffset())
            if isStart: # TODO: Check to see whether multi is the first on the start line or first on the end line
              Some(LineLabel:
                col: label.span.start() - line.offset(),
                label: **label,
                multi: true,
                drawMsg: false,
                # Multi-line spans dont have their messages drawn at the
                # start
              })
            elif isEnd:
              Some(LineLabel:
                  col: label.lastOffset() - line.offset(),
                  label: **label,
                  multi: true,
                  drawMsg: true,
                  # Multi-line spans have their messages drawn at the end
              })
            else:
              None
            }
        })
        .minByKey(|ll| (ll.col, not ll.label.span.start()))

      # Generate a list of labels for this line, along with their label columns
      var lineLabels = multiLabels
          .iter()
          .enumerate()
          .filterMap(|(I, label)|:
            let isStart = line.span().contains(label.span.start())
            let isEnd = line.span().contains(label.lastOffset())
            if isStart and marginLabel.asRef().mapOr(true, |m| **label as *const _ not = m.label as *const _): # TODO: Check to see whether multi is the first on the start line or first on the end line
              Some(LineLabel:
                col: label.span.start() - line.offset(),
                label: **label,
                multi: true,
                drawMsg: false, # Multi-line spans dont have their messages drawn at the start
              })
            elif isEnd:
              Some(LineLabel:
                col: label.lastOffset() - line.offset(),
                label: **label,
                multi: true,
                drawMsg: true, # Multi-line spans have their messages drawn at the end
              })
            else:
                None
            }
          })
          .collect.<Vec<_>>()

      for labelInfo in labels
          .iter()
          .filter(|l| l.label.span.start() >= line.span().start and l.label.span.end() <= line.span().end)
     :
          if matches(labelInfo.kind, LabelKind.Inline):
            lineLabels.push(LineLabel:
              col: match &self.config.labelAttach:
                LabelAttach.Start => labelInfo.label.span.start(),
                LabelAttach.Middle => (labelInfo.label.span.start() + labelInfo.label.span.end()) / 2,
                LabelAttach.End => labelInfo.label.lastOffset(),
              }.max(labelInfo.label.span.start()) - line.offset(),
              label: labelInfo.label,
              multi: false,
              drawMsg: true,
            })
          }
      }

      # Skip this line if we don't have labels for it
      if lineLabels.len() == 0 and marginLabel.isNone():
        let withinLabel = multiLabels
          .iter()
          .any(|label| label.span.contains(line.span().start()))

        if not isEllipsis and withinLabel:
          isEllipsis = true
        else:
          if not self.config.compact and not isEllipsis:
            writeMargin(
              w, idx, false, isEllipsis, false, None, &[], &None)
            write(w, "\n")

          isEllipsis = true
          continue

      else:
        isEllipsis = false

      # Sort the labels by their columns
      lineLabels.sortByKey(
        |ll| (ll.label.order, ll.col, not ll.label.span.start()))

      # Determine label bounds so we know where to put error messages
      let arrowEndSpace = if self.config.compact: 1 else: 2 }
      let arrowLen = lineLabels
        .fold(0, |l, ll| if ll.multi:
            line.len()
        else:
            l.max(ll.label.span.end().saturatingSub(line.offset()))
        }) + arrowEndSpace

      # Should we draw a vertical bar as part of a label arrow on this line?
      proc getVbar(col: int, row: int): Option[int] =
        lineLabels
        # Only labels with notes get an arrow
        .enumerate()
        .filter(|(_, ll)| ll.label.msg.isSome() and marginLabel.asRef().mapOr(true, |m| ll.label as *const _ not = m.label as *const _))
        .find(|(j, ll)| ll.col == col and ((row <= *j and not ll.multi) or (row <= *j and ll.multi)))
        .map(|(_, ll)| ll)

      let getHighlight = |col| marginLabel
        .iter()
        .map(|ll| ll.label)
        .chain(multiLabels.iter().map(|l| **l))
        .chain(lineLabels.iter().map(|l| l.label))
        .filter(|l| l.span.contains(line.offset() + col))
        # Prioritise displaying smaller spans
        .minByKey(|l| (-l.priority, l.span.len()))

      let getUnderline = |col| lineLabels
        .iter()
        .filter(|ll| self.config.underlines
            # Underlines only occur for inline spans (highlighting can
            # occur for all spans)
            and not ll.multi
            and ll.label.span.contains(line.offset() + col))
        # Prioritise displaying smaller spans
        .minByKey(|ll| (-ll.label.priority, ll.label.span.len()))

      # Margin
      writeMargin(
        w, idx, true, isEllipsis, true, None, &lineLabels, &marginLabel)

      # Line
      if not isEllipsis:
        for (col, c) in line.chars().enumerate():
          let color = if let Some(highlight) = getHighlight(col):
            highlight.color

          else:
            self.config.unimportantColor()

          let (c, width) = self.config.charWidth(c, col)
          if c.isWhitespace():
            for _ in 0..width:
              write(w, "{}", c.fg(color))

          else:
            write(w, c.fg(color))

      write(w, "\n")

      # Arrows
      for row in 0..lineLabels.len():
        let lineLabel = &lineLabels[row]
        if not self.config.compact:
          # Margin alternate
          writeMargin(
            w, idx, false, isEllipsis, true,
            Some((row, false)), &lineLabels, &marginLabel)

          # Lines alternate
          var chars = line.chars()
          for col in 0..arrowLen:
            let width = chars.next().mapOr(
              1, |c| self.config.charWidth(c, col).1)

            let vbar = getVbar(col, row)
            let underline = getUnderline(col).filter(|_| row == 0)
            let [c, tail] = if let Some(vbarLl) = vbar:
              let [c, tail] = if underline.isSome():
                # TODO: Is this good?
                if vbarLl.label.span.len() <= 1 or true:
                  [draw.underbar, draw.underline]
                elif line.offset() + col == vbarLl.label.span.start():
                  [draw.ltop, draw.underbar]
                elif line.offset() + col == vbarLl.label.lastOffset():
                  [draw.rtop, draw.underbar]
                else:
                  [draw.underbar, draw.underline]

              else if vbarLl.multi and row == 0 and
                      self.config.multilineArrows:
                [draw.uarrow, ' ']
              else:
                [draw.vbar, ' ']

              [c.fg(vbarLl.label.color), tail.fg(vbarLl.label.color)]
            else if let Some(underlineLl) = underline:
              [draw.underline.fg(underlineLl.label.color) 2]
            else:
              [' '.fg(None) 2]

            for i in 0..width:
              write(w, "{}", if i == 0: c else: tail })

          write(w, "\n")

        # Margin
        writeMargin(
          w, idx, false, isEllipsis, true,
          Some((row, true)), &lineLabels, &marginLabel)
        # Lines
        var chars = line.chars()
        for col in 0..arrowLen:
          let width = chars.next().mapOr(
            1, |c| self.config.charWidth(c, col).1)

          let isHbar = (((col > lineLabel.col) ^ lineLabel.multi)
              or (lineLabel.label.msg.isSome() and lineLabel.drawMsg and col > lineLabel.col)) and lineLabel.label.msg.isSome()
          let [c, tail] = if col == lineLabel.col and lineLabel.label.msg.isSome() and marginLabel.asRef().mapOr(true, |m| lineLabel.label as *const _ not = m.label as *const _):
              [if lineLabel.multi:
                  if lineLabel.drawMsg: draw.mbot else: draw.rbot }
              else:
                  draw.lbot
              }.fg(lineLabel.label.color), draw.hbar.fg(lineLabel.label.color)]
          elif let Some(vbarLl) = getVbar(col, row).filter(
            |_| (col not = lineLabel.col or lineLabel.label.msg.isSome())):

            if not self.config.crossGap and isHbar:
              [draw.xbar.fg(lineLabel.label.color), ' '.fg(lineLabel.label.color)]
            elif isHbar:
              [draw.hbar.fg(lineLabel.label.color) 2]
            else:
              [
                  if vbarLl.multi and row == 0 and self.config.compact:
                      draw.uarrow
                  else:
                      draw.vbar
                  }.fg(vbarLl.label.color),
                  ' '.fg(lineLabel.label.color),
              ]

          elif isHbar:
            [draw.hbar.fg(lineLabel.label.color) 2]
          else:
            [' '.fg(None) 2]

          if width > 0:
            write(w, "{}", c)

          for _ in 1..width:
            write(w, "{}", tail)

        if lineLabel.drawMsg:
          write(w, ":}", repeat(lineLabel.label.msg.asRef()))

        write(w, "\n")

    let isFinalGroup = groupIdx + 1 == groupsLen

    # Help
    if let (Some(note), true) = (self.help, isFinalGroup):
      if not self.config.compact:
        writeMargin(w, 0, false, false, true, Some((0, false)), &[], &None)
        write(w, "\n")

      writeMargin(w, 0, false, false, true, Some((0, false)), &[], &None)
      write(w, "{}::}\n", "Help".fg(self.config.noteColor()), note)

    # Note
    if let (Some(note), true) = (self.note, isFinalGroup):
      if not self.config.compact:
        writeMargin(
          w, 0, false, false, true, Some((0, false)), &[], &None)
        write(w, "\n")

      writeMargin(w, 0, false, false, true, Some((0, false)), &[], &None)
      write(w, "{}::}\n", "Note".fg(self.config.noteColor()), note)

    # Tail of report
    if not self.config.compact:
      if isFinalGroup:
        let finalMargin = concat(
          repeat((draw.hbar, lineNoWidth + 2)), draw.rbot)
        writeLine(w, finalMargin.fg(self.config.marginColor()))

      else:
        writeLine(
          w,
          repeat((' ', lineNoWidth + 2)),
          draw.vbar.fg(self.config.marginColor()))
