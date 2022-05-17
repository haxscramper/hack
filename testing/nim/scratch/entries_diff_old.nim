
  # Attempt to 'uniquely' identify each expected inline annotation
  type UniqueReport = tuple[kind, severity: string, line, column: int]

  # List of expected inline error annotations
  var inlined: Table[UniqueReport, SexpNode]
  # List of the given S-expressions.
  var givenReports: seq[SexpNode]

  # Parse given reports into S-expressions
  for line in splitLines(given.nimout):
    givenReports.add parseSexp(line)

  for inline in expected.inlineErrors:
    let rep = parseSexp(inline.msg)
    let key = (rep[0].getSymbol(), inline.kind, inline.line, inline.col)
    echo "Expecting inline message ", data
    inlined[key] = rep

  # Find all reports that were explicitly added as inlined - they would
  # be excluded from the direct comparison.
  var foundInlined: Table[int, UniqueReport]
  for idx, report in givenReports:
    let pos = (
      # `(ReportKind`
      report[0].getSymbol(),
      # `:severity Hint`
      report.getField("severity"),
      # `:location[1]` for line
      report.getField("location")[1].getNum()
      # `:location[2]` for column
      report.getField("location")[2].getNum()
    )

    if pos in inlined:
      # If there is a report that /exactly/ matches kind, severity,
      # line and location, add it as a known inlined report.
      foundInlined[idx] = pos

  var expIdx = 0
  var giveIdx = 0
  # Iterate over all expected and given reports in parallel. Loop
  # predicate is only on 'given' reports because mismatch in number of
  # expected records would be an error.
  while givenIdx < givenReports.len:
    if givenIdx in foundInlined:
      let diff = diff(givenReports[idx])

  for (expected, given) in zip(
    expected.nimout.split("\n"),
    given.nimout.split("\n")
  ):
    # TODO handle output lines that cannot be parsed with `parseSexp()`
    let diff = diff(expected.parseSexp(), given.parseSexp())

    if 0 < len(diff):
      return false
