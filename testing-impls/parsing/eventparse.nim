import std/[strformat, strutils, sequtils]

type
  EventKind = enum
    evList
    evLBrace
    evRBrace
    evComma,
    evIdent

  Event = object
    case kind*: EventKind
      of evIdent:
        strVal*: string

      else:
        discard

  AstKind = enum
    akList
    akBrace

  Ast = ref object
    kind*: AstKind
    strVal*: string
    subnodes*: seq[Ast]

  Flag = enum
    fInList
    fInBracket


type
  HandlerKind = enum
    hkPush
    hkWrap
    hkTear
    hkPop
    hkPopMany
    hkPopParallel
    hkNext
    hkNextMany
    hkAwait
    hkAwaitMany
    hkTrim
    hkLift
    hkDrop

    hkWhereGroup

  Handler = ref object
    case kind*: HandlerKind
      of hkAwait, hkAwaitMany, hkNext, hkNextMany:
        waitKind*: AstKind
        waitTarget*: ptr seq[Ast]
        found*: bool

      of hkPopParallel:
        popParallelTargets*: seq[(AstKind, ptr seq[Ast])]

      of hkPopMany, hkPop:
        popKind*: AstKind
        popTargets*: ptr seq[Ast]

      of hkLift, hkDrop:
        flag*: Flag

      of hkWhereGroup:
        headActions*: seq[Handler]
        whereBody*: seq[Handler]

      of hkPush:
        newAstKind*: AstKind
        argLists*: seq[seq[Ast]]

      else:
        discard



type Err = object of CatchableError

template die(msg: string): untyped = raise newException(Err, fmt(msg))
template echov(msg: string): untyped =
  block:
    let iinfo {.inject.} = instantiationInfo()
    echo &"{iinfo.line:<3} | ", fmt(msg)





proc isFinished(handler: Handler): bool =
  ## Check whether handler has found required element
  case handler.kind:
    of hkAwait, hkAwaitMany, hkNext, hkNextMany:
      return handler.found

    of hkWhereGroup:
      return allIt(handler.whereBody, isFinished(it))

    else:
      die("Cannot check for finish in {handler.kind}")


proc isImmediate*(handler: Handler): bool =
  ## Whether handler can be immediately executed
  case handler.kind:
    of hkWhereGroup:
      return allIt(handler.whereBody, isImmediate(it))

    of hkAwaitMany, hkAwait:
      false

    else:
      die("{handler.kind}")

proc canExecute*(handler: Handler): bool =
  isImmediate(handler) or isFinished(handler)

proc execOn(
    handler: Handler, event: Event,
    stack: var seq[Ast], waiters: var seq[Handler]
  ) =

  echov "Processing {event.kind} using {handler.kind}"
  case handler.kind:
    of hkWhereGroup:
      # There are two cases - `where` group must first be assigned to
      # corresponding waiters and then collected (group uses
      # `await`/`next`), *or* it has no forward waiters and can be executed
      # immediately
      if canExecute(handler):
        echov "Can immediate execute"
        for action in handler.headActions:
          action.execOn(event, stack, waiters)

    else:
      discard



proc eventParse(events: seq[Event]): Ast =
  var flags: array[Flag, uint8]

  var stack: seq[Ast]

  var handlers: array[EventKind, Handler]

  var waiters: seq[Handler]

  block:
    var pushAction = Handler(
      kind: hkPush,
      newAstKind: akList,
      argLists: @[newSeq[Ast]()]
    )

    handlers[evList] = Handler(
      kind: hkWhereGroup,
      headActions: @[
        Handler(kind: hkLift, flag: fInList),
        pushAction
      ],
      whereBody: @[
        Handler(
          kind: hkAwaitMany, waitKind: akBrace,
          waitTarget: addr pushAction.argLists[0]
        )
      ]
    )

  for ev in events:
    if not isNil(handlers[ev.kind]):
      handlers[ev.kind].execOn(ev, stack, waiters)




proc lex(str: string): seq[Event] =
  for ch in str:
    case ch:
      of '!': result.add Event(kind: evList)
      of '[': result.add Event(kind: evLBrace)
      of ']': result.add Event(kind: evRBrace)
      of ',': result.add Event(kind: evComma)

      of 'a' .. 'z':
        result.add Event(kind: evIdent, strVal: $ch)

      else:
        discard


import hpprint

when isMainModule:
  let text = lex("![]")
  pprint text

  let ast = eventParse(text)
  pprint ast
