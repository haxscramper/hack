import std/[parsejson, streams]

proc newJsonParser*(
    text: string,
    filename: string = "<text>"
  ): JsonParser =

  open(result, newStringStream(text), filename)
  next(result)

proc getStr*(parser: JsonParser): string = parser.str()

proc currentEventToStr*(parser: JsonParser): string =
  result.add $parser.kind
  result.add " "
  result.add $parser.tok
  result.add " "
  result.add():
    case parser.kind:
      of jsonError:       errorMsg(parser)
      of jsonEof:         "[EOF]"
      of jsonString:      parser.getStr()
      of jsonInt:         $parser.getInt()
      of jsonFloat:       $parser.getFloat()
      of jsonTrue:        "true"
      of jsonFalse:       "false"
      of jsonNull:        "null"
      of jsonObjectStart: "{"
      of jsonObjectEnd:   "}"
      of jsonArrayStart:  "["
      of jsonArrayEnd:    "]"

proc displayAt*(parser: JsonParser): string =
  result = $parser.getFilename() & "(" & $parser.getLine &
    ":" & $parser.getColumn & ") "
  result.add currentEventToStr(parser)

var p = newJsonParser("""
{"12": [1, 2, 3]}
""")

iterator eventsUntil*(
    p: var JsonParser,
    endTok: set[TokKind]): JsonEventKind =

  while p.tok notin endTok:
    yield p.kind()
    p.next()

iterator eventsUntil*(
    p: var JsonParser,
    endTok: set[JsonEventKind]): JsonEventKind =

  while p.kind notin endTok:
    yield p.kind()
    p.next()

for ev in p.eventsUntil({tkEof}):
  echo p.currentEventToStr()
