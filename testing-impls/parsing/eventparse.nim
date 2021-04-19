import std/[strformat, strutils, sequtils, tables]
import hmisc/[hdebug_misc, base_errors]
import hmisc/types/colorstring
import hmisc/algo/halgorithm
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
        waitKind: AstKind ## Filter element kinds
        pauseOn: set[Flag] ## Pause waiter when particular set of flags is
                           ## at the top
        waitTarget: ptr seq[Ast] ## Sequence to add elements to
        found: bool ## Waiter finished execution
        onComplete: Handler ## Parent handler to execute if all elements
                            ## have finished completion

      of hkPopParallel:
        ## Remove multiple elements from AST stack assigning to different
        ## targets
        popParallelTargets: seq[(AstKind, ptr seq[Ast])]

      of hkPopMany, hkPop:
        ## Remove elements from AST stack
        popKind: set[AstKind]
        popTarget: ptr seq[Ast]

      of hkLift, hkDrop:
        ## Add or remove flag from the flag stack
        flag: Flag

      of hkWhereGroup:
        ## Toplevel element grouping
        case guarded: bool ## Only trigger when particular flag is active
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
        ## Construct new AST and add it to stack using `argList` as
        ## subnodes
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
    stack: seq[tuple[ast: Ast, level: uint8]]
    waiters: array[AstKind, seq[Handler]]
    flags: seq[Flag]

proc lift(context: var ExecContext, flag: Flag) =
  context.flags.add flag

proc drop(context: var ExecContext, flag: Flag) =
  let val = context.flags.pop
  assert val == flag, &"{val} == {flag}"



proc nextColor[E](used: var set[E]): E =
  const all = { low(E) .. high(E) }
  for item in (all - used):
    result = item
    break

  used.incl result


var table: Table[int, ForegroundColor]
var used: set[ForegroundColor] = {fgDefault, fgBlack}

proc treeRepr(ast: Ast): string =
  proc aux(ast: Ast, level: int): string =
    let pref = "  ".repeat(level)
    let addrInt = cast[int](ast)
    if addrInt notin table:
      table[addrInt] = nextColor(used)

    result &= alignLeft(pref & $ast.kind, 20) & " @" &
      $toColored($addrInt, initStyle(table[addrInt])) & "\n"

    for node in ast.subnodes:
      result &= aux(node, level + 1)

  return aux(ast, 0)

proc treeRepr[T](s: seq[T]): string =
  for item in s:
    result = "-\n"
    for line in split(treeRepr(item), '\n'):
      result &= "  " & line & "\n"

proc execOn(handler: Handler, event: Event, context: var ExecContext)



proc getName(kind: HandlerKind): string =
  const map = toMapArray({
    hkPush: "push",
    hkPop: "pop",
    hkPopMany: "pop*",
    hkAwait: "await",
    hkAwaitMany: "await*",
    hkNext: "next",
    hkNextMany: "next*",
    hkLift: "lift",
    hkDrop: "drop",
    hkWhereGroup: "group"
  })

  return map[kind]

proc `$`(handler: Handler): string =

  proc aux(h: Handler, level: int): seq[string] =
    let pref = "    ".repeat(level)
    case h.kind:
      of hkWhereGroup:
        result.add &"{pref}group"
        for act in h.startActions: result.add aux(act, level + 1)
        for act in h.finishActions: result.add aux(act, level + 1)

        if h.whereBody.len > 0:
          result.add pref & "    " & "where"
          for act in h.whereBody:
            result.add aux(act, level + 2)

      of hkPush:
        result.add &"{pref}push {h.newAstKind}(.. {h.argList.len})"

      of hkTrim:
        result.add &"{pref}trim {h.trimKind}/{h.trimTargetKind}"

      of hkAwait, hkAwaitMany, hkNext, hkNextMany:
        result.add &"{pref}{getName(h.kind)} {h.waitKind}"

      of hkLift, hkDrop:
        result.add &"{pref}{getName(h.kind)} {h.flag}"

      of hkPop, hkPopMany:
        result.add &"{pref}{getName(h.kind)} {h.popKind}"

      of hkPopParallel:
        for (kind, target) in h.popParallelTargets:
          result.add &"{pref}pop# {kind}"

      else:
        raiseImplementError("")

  return aux(handler, 0).join("\n")


proc putAst(target: var seq[Ast], ast: Ast) =
  echov "Adding AST to subnodes"
  echov target.treeRepr()
  echov ast.treeRepr()
  target.add ast

proc putAst(context: var ExecContext, ast: Ast) =
  echov "Putting ast to stack"
  echov context.stack.mapIt(it.ast).treeRepr()
  echov ast.treeRepr()

  var found = false
  for waiter in mitems(context.waiters[ast.kind]):
    if waiter.kind in {hkAwaitMany, hkAwait} and
       waiter.waitKind == ast.kind and
       (context.flags.len > 0 and context.flags[^1] notin waiter.pauseOn):

      waiter.waitTarget[].putAst ast
      waiter.found = true
      found = true
      break

  if not found:
    context.stack.add (ast, context.flags.len.uint8)




proc execOn(handler: Handler, event: Event, context: var ExecContext) =
  case handler.kind:
    of hkWhereGroup:
      # There are two cases - `where` group must first be assigned to
      # corresponding waiters and then collected (group uses
      # `await`/`next`), *or* it has no forward waiters and can be executed
      # immediately
      if event.kind == evAwaitFinish:
        for action in handler.finishActions:
          action.execOn(event, context)

      else:
        for action in handler.startActions:
          action.execOn(event, context)

        if canExecute(handler):
          # Finish all trailing actions
          for action in handler.whereBody:
            action.execOn(event, context)

          for action in handler.finishActions:
            action.execOn(event, context)

        else:
          for action in mitems(handler.whereBody):
            if action.isWaiter():
              action.onComplete = handler
              context.waiters[action.waitKind].add action

    of hkLift:
      context.lift handler.flag

    of hkDrop:
      context.drop handler.flag

    of hkPopMany:
      echov "Pop many"
      echov "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv"
      if context.stack.len > 0:
        echov context.stack.mapIt(it.ast).treeRepr()
        let level = context.stack[^1].level

        while len(context.stack) > 0 and
              level > context.flags.len.uint8 and
              context.stack[^1].ast.kind in handler.popKind:
          echov "popping element from stack"
          handler.popTarget[].putAst context.stack.pop().ast

      echov "Stack after popping"
      echov context.stack.mapIt(it.ast).treeRepr()
      echov "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"

    of hkPush:
      context.putAst Ast(
        kind: handler.newAstKind, subnodes: handler.argList)

      handler.argList = @[]

    of hkCloseAll:
      for group in mitems(context.waiters):
        for waiter in mitems(group):
          waiter.execOn(event, context)

    of hkAwaitMany:
      if event.kind == evFinish:
        handler.onComplete.execOn(event, context)

    of hkTrim:
      var target = -1
      let kind = handler.trimTargetKind
      for idx, waiter in context.waiters[kind]:
        if waiter.kind == handler.trimKind:
          target = idx
          break

      if target == -1:
        raiseLogicError(
          &"Cannot trim {handler.trimKind}/{handler.trimTargetKind} - no ongoing action")

      context.waiters[kind][target].onComplete.execOn(
        Event(kind: evAwaitFinish), context)

      context.waiters[kind].delete(target)

    else:
      raiseImplementKindError(handler)



proc eventParse(events: seq[Event]): seq[Ast] =
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
          waitTarget: addr pushAction.argList,
          pauseOn: {fInBrace}
        )
      ]
    )]

  handlers[evLBrace] = @[Handler(
    kind: hkWhereGroup,
    startActions: @[Handler(
      kind: hkLift,
      flag: fInBrace
    )])]

  block:
    var pushAction = Handler(
      kind: hkPush,
      newAstKind: akBrace,
      argList: newSeq[Ast]()
    )

    handlers[evRBrace].add Handler(
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
    )

  block:
    handlers[evRBrace].add Handler(
      guarded: true,
      guardFlag: fInList,
      kind: hkWhereGroup,
      startActions: @[
        Handler(kind: hkDrop, flag: fInList),
        Handler(kind: hkTrim, trimTargetKind: akBrace, trimKind: hkAwaitMany)
      ]
    )

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

  return context.stack.mapIt(it.ast)




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
  let text = lex("![[[][]]]")

  let ast = eventParse(text)
  # pprint ast, showPath = true
  # pprint ast #, ignore = @["**/strVal"]
  for ast in ast:
    echo ast.treeRepr()
