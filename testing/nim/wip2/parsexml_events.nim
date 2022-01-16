import std/[parsexml, streams, strutils, strformat]


proc newXmlParser*(text: string, traceNext: bool = false): XmlParser =
  var stringStream = newStringStream(text)
  open(result, stringStream, "<text>")

proc strVal*(parser: XmlParser, ind: var int): string =
  result = &"{parser.getLine:>3}:{parser.getColumn:<3}" & "|  ".repeat(ind)
  case parser.kind:
    of {xmlAttribute}:
      result &= parser.attrKey() & " " & parser.attrValue()
    of xmlCharData, xmlWhitespace, xmlComment, xmlCData, xmlSpecial:
      result &= parser.charData()

    of xmlElementStart:
      result &= "<" & parser.elementName() & ">"
      inc ind

    of xmlElementOpen:
      result &= "<" & parser.elementName()
      inc ind

    of xmlElementClose:
      result &= ">"

    of xmlElementEnd:
      result &= "</" & parser.elementName() & ">"
      dec ind

    else:
      discard

proc test(str: string) =
  var parser = newXmlParser(str)
  var ind = 0
  while true:
    parser.next()
    echo alignLeft(($parser.kind)[3..^1], 15), " ", parser.strVal(ind)
    if parser.kind == xmlEof:
      return
test:
  """
<main id="0">
  <used id="1">
    <used id="0"/>
    <used id="2">
      <field>0</field>
    </used>
    <used id="3">
      <field>0</field>
    </used>
    <used/>
    <field>0</field>
  </used>
  <field>0</field>
</main>
"""

test("/tmp/q.xml".readFile())

# test:
#   """
# <main id="0">
#   <used id="1">
#     <only-attr id="0"/>
#     <with-nested id="2"> <field>0</field> </with-nested>
#     <single-used/>
#     <regular-fields>0</regular-fields>
#   </used>
#   <field>0</field>
# </main>
# """
