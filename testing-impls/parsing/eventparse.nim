import std/[strformat, strutils, sequtils]
import hmisc/[hdebug_misc, base_errors]
import hpprint

startHax()

type
  EventKind = enum
    evList
    evLBrace
    evRBrace
    evComma,
    evIdent

    evFinish ## Finish of the input stream. Special event kind
    evAwaitFinish ## `await*` or `next*` handler has finished execution

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
    fInBrace


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
    hkCloseAll

  Handler = ref object
    case kind: HandlerKind
      of hkAwait, hkAwaitMany, hkNext, hkNextMany:
        waitKind: AstKind
        waitTarget: ptr seq[Ast]
        found: bool
        onComplete: Handler ## Parent handler to execute if all elements
                            ## have finished completion

      of hkPopParallel:
        popParallelTargets: seq[(AstKind, ptr seq[Ast])]

      of hkPopMany, hkPop:
        popKind: set[AstKind]
        popTarget: ptr seq[Ast]

      of hkLift, hkDrop:
        flag: Flag

      of hkWhereGroup:
        case guarded: bool
          of true:
            guardFlag: Flag

          else:
            discard

        startActions: seq[Handler] ## Actions executed immediately when
                                   ## group starts
        finishActions: seq[Handler] ## Actions executed after groups
                                    ## `where` completes
        whereBody: seq[Handler]

      of hkPush:
        newAstKind: AstKind
        argList: seq[Ast]

      of hkTrim:
        ## `trim` is the action to stop any other ongoing actions.
        trimKind: HandlerKind ## Action kind to trim
        trimTargetKind: AstKind ## Kind of the *action target*

      else:
        discard



type Err = object of CatchableError

proc isFinished(handler: Handler): bool =
  ## Check whether handler has found required element
  case handler.kind:
    of hkAwait, hkAwaitMany, hkNext, hkNextMany:
      return handler.found

    of hkWhereGroup:
      return allIt(handler.whereBody, isFinished(it))

    else:
      raiseImplementError(&"Cannot check for finish in {handler.kind}")


proc isImmediate*(handler: Handler): bool =
  ## Whether handler can be immediately executed
  case handler.kind:
    of hkWhereGroup:
      return allIt(handler.whereBody, isImmediate(it))

    of hkAwaitMany, hkAwait:
      false

    of hkPop, hkPopMany, hkPopParallel:
      true

    else:
      raiseImplementKindError(handler)

proc isWaiter*(handler: Handler): bool =
  handler.kind in {hkAwait, hkAwaitMany, hkNext, hkNextMany}

proc canExecute*(handler: Handler): bool =
  isImmediate(handler) or isFinished(handler)

type
  ExecContext = object
    stack: seq[Ast]
    waiters: seq[Handler]
    flags: seq[Flag]

proc lift(context: var ExecContext, flag: Flag) =
  echov "Lifted", flag
  context.flags.add flag

proc drop(context: var ExecContext, flag: Flag) =
  let val = context.flags.pop
  assert val == flag, &"{val} == {flag}"

proc execOn(handler: Handler, event: Event, context: var ExecContext)

proc putAst(context: var ExecContext, ast: Ast) =
  for waiter in mitems(context.waiters):
    if waiter.kind in {hkAwaitMany, hkAwait} and
       waiter.waitKind == ast.kind:

      echov "Has waiter for ast kind", ast.kind, "pushing to wait list"

      waiter.waitTarget[].add ast
      waiter.found = true

    else:
      context.stack.add ast


proc execTrim(handler: var Handler) =
  discard

proc execOn(handler: Handler, event: Event, context: var ExecContext) =

  echov event.kind, handler.kind
  case handler.kind:
    of hkWhereGroup:
      # There are two cases - `where` group must first be assigned to
      # corresponding waiters and then collected (group uses
      # `await`/`next`), *or* it has no forward waiters and can be executed
      # immediately
      for action in handler.startActions:
        action.execOn(event, context)

      if canExecute(handler):
        # Finish all trailing actions
        for action in handler.whereBody:
          action.execOn(event, context)

        echov "Can immediate execute"
        for action in handler.finishActions:
          action.execOn(event, context)

      else:
        for action in handler.whereBody:
          if action.isWaiter():
            context.waiters.add action

    of hkLift:
      context.lift handler.flag

    of hkDrop:
      context.drop handler.flag

    of hkPopMany:
      while len(context.stack) > 0 and
            context.stack[^1].kind in handler.popKind:
        handler.popTarget[].add context.stack.pop()

    of hkPush:
      context.putAst Ast(
        kind: handler.newAstKind, subnodes: handler.argList)

    of hkCloseAll:
      for waiter in mitems(context.waiters):
        waiter.execOn(event, context)

    of hkAwaitMany:
      if event.kind == evFinish:
        handler.onComplete.execOn(event, context)

    of hkTrim:
      echov "Executing trim action"
      var target = -1
      for idx, waiter in context.waiters:
        if waiter.kind == handler.trimKind and
           waiter.waitKind == handler.trimTargetKind:
          target = idx
          break

      if target == -1:
        raiseLogicError(
          &"Cannot trim {handler.trimKind}/{handler.trimTargetKind} - no ongoing action")

      context.waiters[target].execTrim()
      context.waiters.delete(target)



    else:
      raiseImplementKindError(handler)



proc eventParse(events: seq[Event]): Ast =
  var handlers: array[EventKind, seq[Handler]]
  var context: ExecContext

  block:
    var pushAction = Handler(
      kind: hkPush,
      newAstKind: akList,
      argList: newSeq[Ast]()
    )

    handlers[evList] = @[Handler(
      kind: hkWhereGroup,
      startActions: @[
        Handler(kind: hkLift, flag: fInList),
      ],
      finishActions: @[
        pushAction
      ],
      whereBody: @[
        Handler(
          kind: hkAwaitMany, waitKind: akBrace,
          waitTarget: addr pushAction.argList
        )
      ]
    )]

  handlers[evLBrace] = @[Handler(
    kind: hkWhereGroup,
    startActions: @[Handler(
      kind: hkLift,
      flag: fInList
    )])]

  block:
    var pushAction = Handler(
      kind: hkPush,
      newAstKind: akBrace,
      argList: newSeq[Ast]()
    )

    handlers[evRBrace] = @[Handler(
      guarded: true,
      guardFlag: fInBrace,
      kind: hkWhereGroup,
      startActions: @[
        Handler(kind: hkDrop, flag: fInBrace),
      ],
      finishActions: @[
        pushAction
      ],
      whereBody: @[
        Handler(
          kind: hkPopMany, popKind: {akList, akBrace},
          popTarget: addr pushAction.argList
        )
      ]
    )]

  block:
    handlers[evRBrace] = @[Handler(
      guarded: true,
      guardFlag: fInList,
      kind: hkWhereGroup,
      startActions: @[
        Handler(kind: hkDrop, flag: fInList),
        Handler(kind: hkTrim, trimTargetKind: akBrace, trimKind: hkAwaitMany)
      ]
    )]

  handlers[evFinish] = @[Handler(
    kind: hkWhereGroup,
    startActions: @[Handler(kind: hkCloseAll)]
  )]

  for ev in events:
    for handler in handlers[ev.kind]:
      if not handler.guarded or (
        context.flags.len > 0 and context.flags[^1] == handler.guardFlag
      ):
        handler.execOn(ev, context)




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

  # result.add Event(kind: evFinish)


import hpprint

when isMainModule:
  let text = lex("![]")
  pprint text

  let ast = eventParse(text)
  pprint ast
