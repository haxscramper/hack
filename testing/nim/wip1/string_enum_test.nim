import re

type
  DirKind = enum
    dkNone = ""
    dkAuthor = "author"
    dkAuthors = "authors"
    dkCode = "code"
    dkCodeBlock = "code-block"
    dkContainer = "container"
    dkContents = "contents"
    dkFigure = "figure"
    dkImage = "image"
    dkInclude = "include"
    dkIndex = "index"
    dkRaw = "raw"
    dkTitle = "title"
    dkFootnote = ""

proc getKind(s: string): DirKind =
  for val in low(DirKind) .. high(DirKind):
    if $val == s:
      return val

  if s =~ re r"[\d]":
    return dkFootnote

# TODO how to print original value?
echo getKind("[1]")
