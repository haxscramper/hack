import hargparse, options, strutils, sets, sequtils
import hmisc/[helpers, halgorithm, hpprint]

let keywords = stdin.readLine().split(",").toHashSet()
var elements: seq[tuple[kwds: seq[string], other: seq[string]]]
var ident = 0

func identation(s: string): int =
  for c in s:
    if c in Whitespace:
      inc result
    else:
      break


for line in stdin.lines():
  if line.len < 1:
    continue

  ident = max(ident, line.identation())
  let words = line.split(" ")
  var lineStart = true
  elements.add((emptySeq[string](), emptySeq[string]()))
  for word in words:
    if word == "#" or word.isEmptyOrWhitespace():
      discard
    elif word in keywords:
      if lineStart:
        lineStart = false
        elements.add((@[word], emptySeq[string]()))
      else:
        elements.last[0].add word
    else:
      lineStart = false
      elements.last[1].add word

# pprint elements

let text = elements.mapPairs((lhs.joinw(), rhs.joinw()))
let todoWidth = text.mapPairs(lhs.len).max()

let comment = text.wrapTwoColumns(
  widthColLimits = (todoWidth, -1)
).join("\n").split("\n").mapIt(" ".repeat(ident) & "#" & it).joinl

echo comment
